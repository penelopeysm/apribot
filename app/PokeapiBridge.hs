{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module PokeapiBridge
  ( ha,
    randomAbility,
    Game (..),
    em,
    randomMoves,
    Parent (..),
    order,
    EggMove (..),
    speciesNameToRealName,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData, force)
import Control.Exception (throwIO, try, evaluate)
import Control.Monad (forM, replicateM, (>=>))
import Data.List (partition, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Paths_apribot (getDataFileName)
import Pokeapi
import System.Process (readProcess)
import System.Random (randomRIO)

terror :: Text -> IO a
terror = throwIO . PokeException

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- * Hidden abilities

-- | Generate a random ability.
randomAbility :: IO (Text, Text)
randomAbility = do
  abId :: Int <- randomRIO (1, 358)
  abty <- get (T.pack $ show abId)
  pure (getAbilityEngName abty, getAbilityDescription abty)

-- | Get a list of all possible abili$ties of a Pokemon. The Bool indicates
-- whether the ability is a hidden ability (True corresponds to a HA).
getAbilitiesAndHidden :: Pokemon -> IO [(Ability, Bool)]
getAbilitiesAndHidden pkmn = do
  let pAbilities = pokemonAbilities pkmn
  case pAbilities of
    [] -> throwIO $ PokeException $ "No abilities found for Pokemon '" <> pokemonName pkmn <> "'. (This should not happen.)"
    _ -> do
      abilities <- mapConcurrently (resolve . paAbility) pAbilities
      let hiddens = map paIsHidden pAbilities
      pure $ zip abilities hiddens

getAbilityDescription :: Ability -> Text
getAbilityDescription abty =
  let engFlavorTexts = filter ((== "en") . name . aftLanguage) (abilityFlavorTextEntries abty)
   in -- We try to get SV > SwSh > BDSP > USUM, then fallback to whatever we can find
      case filter ((== gameToText SV) . name . aftVersionGroup) engFlavorTexts of
        x : _ -> aftFlavorText x
        [] -> case filter ((== gameToText SwSh) . name . aftVersionGroup) engFlavorTexts of
          x : _ -> aftFlavorText x
          [] -> case filter ((== gameToText BDSP) . name . aftVersionGroup) engFlavorTexts of
            x : _ -> aftFlavorText x
            [] -> case filter ((== gameToText USUM) . name . aftVersionGroup) engFlavorTexts of
              x : _ -> aftFlavorText x
              [] -> case engFlavorTexts of
                x : _ -> aftFlavorText x
                [] -> ""

getAbilityEngName :: Ability -> Text
getAbilityEngName abty =
  let engNames = filter ((== "en") . name . nameLanguage) (abilityNames abty)
   in case engNames of
        [] -> "**You managed to break the bot. Congratulations!**"
        (n : _) -> nameName n

-- | Get the hidden ability of a Pokemon, if it exists.
haPokemon :: Pokemon -> IO (Maybe (Text, Text))
haPokemon pkmn = do
  abilities' <- getAbilitiesAndHidden pkmn
  let (ha', na') = partition snd abilities'
  case ha' of
    [] -> pure Nothing
    [(a, _)] ->
      -- Need to check that it's not a duplicate. Some Pokemon (especially in
      -- Gen 9) have their NAs also listed as HAs. See also
      -- https://github.com/PokeAPI/pokeapi/issues/907
      if abilityName a `elem` map (abilityName . fst) na'
        then pure Nothing
        else pure $ Just (getAbilityEngName a, getAbilityDescription a)
    _ -> terror $ "Multiple hidden abilities found for Pokemon " <> pokemonName pkmn <> ": " <> tshow ha' <> "."

haSpecies :: PokemonSpecies -> IO [(Text, Maybe (Text, Text))]
haSpecies species = do
  forM (psVarieties species) $ \var -> do
    pkmn <- resolve (psvPokemon var)
    ha' <- haPokemon pkmn
    pure (name (psvPokemon var), ha')

-- | Get the hidden ability of a Pokemon. First try to resolve it as a Pokemon.
-- If that fails, try to resolve it as a PokemonSpecies.
ha :: Text -> IO [(Text, Maybe (Text, Text))]
ha nm = do
  pkmn <- try $ get @Pokemon nm
  case pkmn of
    Right p -> do
      haDesc <- haPokemon p
      pure [(pokemonName p, haDesc)]
    Left (_ :: PokeException) -> do
      -- No need to catch here, let exceptions bubble up
      species <- get @PokemonSpecies nm
      haSpecies species

-- * Egg moves

data Game = USUM | SwSh | BDSP | SV deriving (Eq, Ord, Show)

gameToText :: Game -> Text
gameToText USUM = "ultra-sun-ultra-moon"
gameToText SwSh = "sword-shield"
gameToText BDSP = "brilliant-diamond-and-shining-pearl"
gameToText SV = "scarlet-violet"

data Parent
  = LevelUpParent {lupPkmnName :: Text, lupOrder :: Int, lupLevel :: Int}
  | BreedParent {bpPkmnName :: Text, bpOrder :: Int}
  | BothParent {bothPkmnName :: Text, bothOrder :: Int, bothLevel :: Int}
  deriving (Eq, Ord, Show, Generic)

instance NFData Parent

order :: Parent -> Int
order (LevelUpParent _ o _) = o
order (BreedParent _ o) = o
order (BothParent _ o _) = o

-- | Identify whether a given Pokemon is a compatible parent for a given egg
-- move.
identifyParent ::
  -- | Game we are looking in
  Game ->
  -- | Name of the egg move
  Text ->
  -- | Names of the egg groups the potential baby is in
  [Text] ->
  -- | URL pointing to the evolution chain of the potential baby. We use the URL
  -- here to save on having to make another API call
  Text ->
  -- | The prospective parent
  Pokemon ->
  -- | What type of parent the Pokemon is, if at all
  IO (Maybe Parent)
identifyParent game moveName' eggGroupNames ecUrl' pkmn = do
  species <- resolve (pokemonSpecies pkmn)
  speciesEggGroups <- getEggGroups species
  if (game /= SV && all (`notElem` eggGroupNames) speciesEggGroups) -- Wrong egg group
    || (game /= SV && psGenderRate species == 8 && url (psEvolutionChain species) /= ecUrl') -- Female-only
    then pure Nothing
    else do
      let theMove = filter (\mv -> name (pmMove mv) == moveName') (pokemonMoves pkmn)
          name' = pokemonName pkmn
          order' = pokemonOrder pkmn
      pure $ case theMove of
        [] -> Nothing -- Pokemon does not learn this move
        [mv] ->
          -- check if it learns via level up
          let vgd = pmVersionGroupDetails mv
              maybeLevelUpLevel = case filter
                ( \pmv ->
                    name (pmvMoveLearnMethod pmv) == "level-up"
                      && name (pmvVersionGroup pmv) == gameToText game
                )
                vgd of
                [pmv] -> Just (pmvLevelLearnedAt pmv)
                _ -> Nothing
              isBreedParent = case filter
                ( \pmv ->
                    name (pmvMoveLearnMethod pmv) == "egg"
                      && name (pmvVersionGroup pmv) == gameToText game
                )
                vgd of
                [_] -> True
                _ -> False
           in case (maybeLevelUpLevel, isBreedParent) of
                (Just lvl, False) -> Just $ LevelUpParent name' order' lvl
                (Nothing, True) -> Just $ BreedParent name' order'
                (Just lvl, True) -> Just $ BothParent name' order' lvl
                (Nothing, False) -> Nothing
        _ -> Nothing -- Move was found twice in the list, shouldn't happen

getParents :: Game -> Move -> [Text] -> Text -> IO [Parent]
getParents game mv eggGroupNames ecUrl' = do
  -- TODO: BDSP parents cannot be checked because of missing PokeAPI data
  let learners = moveLearnedByPokemon mv
  let getParent learner = do
        pkmn <- resolve learner
        identifyParent game (moveName mv) eggGroupNames ecUrl' pkmn
  parents <- mapConcurrentlyStrict getParent learners
  pure $ catMaybes parents

getMoveEnglishName :: Move -> Maybe Text
getMoveEnglishName move =
  let names = moveNames move
   in case filter (\n -> name (nameLanguage n) == "en") names of
        [] -> Nothing
        (n : _) -> Just (nameName n)

randomMoves :: IO [(Text, Text)]
randomMoves = do
  allMoves <- gets @Move (Just 100000) Nothing
  numberOfMoves :: Int <- randomRIO (2, 6)
  -- randomly generate that number of moves
  moveIndices :: [Int] <- replicateM numberOfMoves (randomRIO (0, length allMoves - 1))
  moves <- mapConcurrently (\i -> resolve (allMoves !! i)) moveIndices
  pure $
    sort $
      mapMaybe
        ( \m ->
            case getMoveEnglishName m of
              Nothing -> Nothing
              Just enName -> Just (enName, getMoveFlavorText SV m)
        )
        moves

data EggMove = EggMove
  { emName :: Text,
    emFlavorText :: Text,
    emParents :: [Parent]
  }
  deriving (Eq, Ord, Show)

isEggMove :: Game -> PokemonMove -> Bool
isEggMove game m =
  let vgDetails = pmVersionGroupDetails m
   in any
        ( \vgd ->
            name (pmvMoveLearnMethod vgd) == "egg"
              && name (pmvVersionGroup vgd) == gameToText game
        )
        vgDetails

getEggGroups :: PokemonSpecies -> IO [Text]
getEggGroups species =
  if not (psIsBaby species)
    then pure $ map name (psEggGroups species)
    else do
      chain <- resolve (psEvolutionChain species)
      let evolvesTo = clSpecies . head . clEvolvesTo . ecChain $ chain
      evolution <- resolve evolvesTo
      pure $ map name (psEggGroups evolution)

isValidMove :: MoveFlavorText -> Bool
isValidMove mft =
  not ("this move is forgotten" `T.isInfixOf` mftFlavorText mft)

isValidMoveInGame :: Game -> MoveFlavorText -> Bool
isValidMoveInGame g mft = name (mftVersionGroup mft) == gameToText g && isValidMove mft

getMoveFlavorText :: Game -> Move -> Text
getMoveFlavorText game move =
  -- We try to get the game we're using > SV > SwSh > BDSP > USUM, then fallback to whatever we can find
  let engFlavorTexts = filter ((== "en") . name . mftLanguage) (moveFlavorTextEntries move)
      ft' = case filter (isValidMoveInGame game) engFlavorTexts of
        x : _ -> mftFlavorText x
        [] -> case filter (isValidMoveInGame SV) engFlavorTexts of
          x : _ -> mftFlavorText x
          [] -> case filter (isValidMoveInGame SwSh) engFlavorTexts of
            x : _ -> mftFlavorText x
            [] -> case filter (isValidMoveInGame BDSP) engFlavorTexts of
              x : _ -> mftFlavorText x
              [] -> case filter (isValidMoveInGame USUM) engFlavorTexts of
                x : _ -> mftFlavorText x
                [] -> case filter isValidMove engFlavorTexts of
                  x : _ -> mftFlavorText x
                  [] -> "(no description found)"
   in T.unwords (T.lines ft')

em :: Game -> Text -> IO [EggMove]
em game pkmn = do
  eitherPoke <- try $ get @Pokemon pkmn
  case eitherPoke of
    Left (e :: PokeException) -> throwIO e
    Right poke -> do
      species <- resolve (pokemonSpecies poke)
      eggGroupNames <- getEggGroups species
      actualMoves <- case game of
        -- PokeAPI doesn't have BDSP egg moves so we scrape from PokemonDB
        -- instead (and pray that the moves come out with the same names as in
        -- PokeAPI)
        -- Hopefully this is temporary. The long term aim would be to add BDSP
        -- egg moves to PokeAPI. I opened an issue:
        -- https://github.com/PokeAPI/pokeapi/issues/914
        BDSP -> do
          bdspEmPath <- getDataFileName "python/bdsp_em.py"
          bdspEms <- try $ readProcess bdspEmPath [T.unpack pkmn] ""
          case bdspEms of
            Left (_ :: IOError) -> throwIO $ PokeException "Failed to get BDSP egg moves"
            Right ems -> mapM get (T.words . T.pack $ ems)
        _ -> do
          let moves = pokemonMoves poke
          let moves' = filter (isEggMove game) moves
          mapM (resolve . pmMove) moves'
      let mkEggMove :: Move -> IO (Maybe EggMove)
          mkEggMove m = do
            case getMoveEnglishName m of
              Just engName -> do
                parents <- getParents game m eggGroupNames (url (psEvolutionChain species))
                let flavorText = getMoveFlavorText game m
                pure $ Just $ EggMove engName flavorText parents
              Nothing -> pure Nothing
      ems <- mapM mkEggMove actualMoves
      pure $ sort $ catMaybes ems

-- * General

mapConcurrentlyStrict :: (Control.DeepSeq.NFData b) => (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyStrict f = mapConcurrently (f >=> (evaluate . force))

capitaliseFirst :: Text -> Text
capitaliseFirst t = T.toUpper (T.take 1 t) <> T.drop 1 t

speciesNameToRealName :: Text -> Text
speciesNameToRealName t =
  case t of
    "jangmo-o" -> "Jangmo-o"
    t' ->
      T.intercalate "-"
        . map capitaliseFirst
        . T.splitOn "-"
        . T.replace "farfetchd" "farfetch'd"
        . T.replace "sirfetchd" "sirfetch'd"
        . T.replace "mr-mime" "mr. Mime"
        . T.replace "mr-rime" "mr. Rime"
        . T.replace "flabebe" "flabébé"
        $ t'
