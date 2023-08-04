{-# LANGUAGE TypeApplications #-}

module PokeapiBridge (haSpecies, randomAbility, Game (..), randomMoves, em) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Data.List (partition, sort)
import Data.Maybe (mapMaybe)
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

-- | Get a list of all possible abilities of a Pokemon. The Bool indicates
-- whether the ability is a hidden ability (True corresponds to a HA).
getAbilitiesAndHidden :: Text -> IO [(Ability, Bool)]
getAbilitiesAndHidden p = do
  pAbilities <- pokemonAbilities <$> get p
  case pAbilities of
    [] -> throwIO $ PokeException $ "No abilities found for Pokemon '" <> p <> "'. (This should not happen.)"
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
ha :: Text -> IO (Maybe (Text, Text))
ha p = do
  abilities' <- getAbilitiesAndHidden p
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
    _ -> terror $ "Multiple hidden abilities found for Pokemon " <> p <> ": " <> tshow ha' <> "."

haSpecies :: Text -> IO [(Text, Maybe (Text, Text))]
haSpecies ps = do
  species <- get ps
  case psVarieties species of
    [] -> terror $ "No varieties found for Pokemon species '" <> ps <> "'."
    [var] -> do
      let name' = name (psvPokemon var)
      ha' <- ha name'
      pure [(name', ha')]
    vars ->
      mapM
        ( \var -> do
            let name' = name (psvPokemon var)
            ha' <- ha name'
            pure (name', ha')
        )
        vars

-- * Egg moves

data Game = USUM | SwSh | BDSP | SV

data LevelUpParent = LUP {lupName :: Text, lupLevel :: Int}

newtype BreedParent = BP {bpName :: Text}

data EggMove = EM {emName :: Text, levelParents :: [LevelUpParent], breedParents :: [BreedParent]}

-- getLevelUpParents :: Game -> Text -> IO [LevelUpParent]
-- getLevelUpParents game move = do
--   move' <- get move
--   let lup = moveLevelUpLearnMove move'
--   case lup of
--     Nothing -> pure []
--     Just lup' -> do
--       let lup'' = filter (\l -> name (lupVersionGroup l) == gameToText game) lup'
--       mapM
--         ( \l -> do
--             let lupName' = name (lupMove l)
--             lupLevel' <- get (name (lupMove l))
--             pure $ LUP lupName' (lupLevel l)
--         )
--         lup''

gameToText :: Game -> Text
gameToText USUM = "ultra-sun-ultra-moon"
gameToText SwSh = "sword-shield"
gameToText BDSP = "brilliant-diamond-shining-pearl"
gameToText SV = "scarlet-violet"

getMoveEnglishName :: Move -> Maybe Text
getMoveEnglishName move =
  let names = moveNames move
   in case filter (\n -> name (nameLanguage n) == "en") names of
        [] -> Nothing
        (n : _) -> Just (nameName n)

randomMoves :: IO [Text]
randomMoves = do
  allMoves <- gets @Move (Just 100000) Nothing
  numberOfMoves :: Int <- randomRIO (2, 6)
  -- randomly generate that number of moves
  moveIndices :: [Int] <- replicateM numberOfMoves (randomRIO (0, length allMoves - 1))
  moves <- mapConcurrently (\i -> resolve (allMoves !! i)) moveIndices
  pure $ sort $ mapMaybe getMoveEnglishName moves

isEggMove :: Game -> PokemonMove -> Bool
isEggMove game m =
  let vgDetails = pmVersionGroupDetails m
   in any
        ( \vgd ->
            name (pmvMoveLearnMethod vgd) == "egg"
              && name (pmvVersionGroup vgd) == gameToText game
        )
        vgDetails

em :: Game -> Text -> IO [Text]
em game pkmn = do
  moves <- pokemonMoves <$> get pkmn
  let moves' = filter (isEggMove game) moves
  actualMoves <- mapM (resolve . pmMove) moves'
  pure $ sort $ mapMaybe getMoveEnglishName actualMoves
