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
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO, try)
import Control.Monad (forM, replicateM)
import Data.List (partition, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Pokeapi
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
gameToText BDSP = "brilliant-diamond-shining-pearl"
gameToText SV = "scarlet-violet"

data Parent
  = LevelUpParent {lupPkmnName :: Text, lupOrder :: Int, lupLevel :: Int}
  | BreedParent {bpPkmnName :: Text, bpOrder :: Int}
  | BothParent {bothPkmnName :: Text, bothOrder :: Int, bothLevel :: Int}
  deriving (Eq, Ord, Show)

order :: Parent -> Int
order (LevelUpParent _ o _) = o
order (BreedParent _ o) = o
order (BothParent _ o _) = o

identifyParent :: Game -> Text -> [Text] -> Pokemon -> IO (Maybe Parent)
identifyParent game moveName' eggGroupNames pkmn = do
  species <- resolve (pokemonSpecies pkmn)
  speciesEggGroups <- getEggGroups species
  if game /= SV && all (`notElem` eggGroupNames) speciesEggGroups
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

getParents :: Game -> Move -> [Text] -> IO [Parent]
getParents game mv eggGroupNames = do
  learners <- mapConcurrently resolve (moveLearnedByPokemon mv)
  parents <- mapConcurrently (identifyParent game (moveName mv) eggGroupNames) learners
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

isValidMoveInGame :: Game -> MoveFlavorText -> Bool
isValidMoveInGame g mft =
  name (mftVersionGroup mft) == gameToText g
    && not ("This move can't be used" `T.isPrefixOf` mftFlavorText mft)

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
                [] -> case engFlavorTexts of
                  x : _ -> mftFlavorText x
                  [] -> "(no description found)"
   in T.unwords (T.lines ft')

em :: Game -> Text -> IO [EggMove]
em game pkmn = do
  poke <- get @Pokemon pkmn
  let moves = pokemonMoves poke
  species <- resolve (pokemonSpecies poke)
  eggGroupNames <- getEggGroups species
  let moves' = filter (isEggMove game) moves
  actualMoves <- mapM (resolve . pmMove) moves'
  let mkEggMove :: Move -> IO (Maybe EggMove)
      mkEggMove m = do
        case getMoveEnglishName m of
          Just engName -> do
            parents <- getParents game m eggGroupNames
            let flavorText = getMoveFlavorText game m
            pure $ Just $ EggMove engName flavorText parents
          Nothing -> pure Nothing
  ems <- mapM mkEggMove actualMoves
  pure $ sort $ catMaybes ems
