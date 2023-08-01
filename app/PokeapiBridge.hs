module PokeapiBridge (haSpecies, randomAbility) where

import Control.Exception (throwIO)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import Pokeapi
import System.Random (randomRIO)

terror :: Text -> IO a
terror = throwIO . PokeException

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Generate a random ability.
randomAbility :: IO Text
randomAbility = do
  abId :: Int <- randomRIO (0, 358)
  abty <- get (T.pack $ show abId)
  let englishName = filter (\n -> name (nameLanguage n) == "en") (abilityNames abty)
  pure $ case englishName of
    [] -> "You managed to break the bot. Congratulations!"
    (n : _) -> nameName n

-- | Get the (correctly-hyphenated / capitalised) name of an ability in a given
-- language. Use "en" for English.
getAbilityNameInLang :: Text -> Text -> IO (Maybe Text)
getAbilityNameInLang abty lang = do
  abty' <- get abty
  let names = abilityNames abty'
  case filter (\n -> name (nameLanguage n) == lang) names of
    [] -> pure Nothing
    (n : _) -> pure $ Just (nameName n)

-- | Get a list of all possible abilities of a Pokemon. The Bool indicates
-- whether the ability is a hidden ability (True corresponds to a HA).
abilities :: Text -> IO [(Text, Bool)]
abilities p = do
  abilities' <- pokemonAbilities <$> get p
  case abilities' of
    [] -> throwIO $ PokeException $ "No abilities found for Pokemon '" <> p <> "'. (This should not happen.)"
    _ -> do
      mapM
        ( \a -> do
            let abilityName = name (paAbility a)
            englishName <- getAbilityNameInLang abilityName "en"
            case englishName of
              Nothing -> terror $ "No English name found for ability '" <> name (paAbility a) <> "'. (This should not happen.)"
              Just n -> pure (n, paIsHidden a)
        )
        abilities'

-- | Get the hidden ability of a Pokemon, if it exists.
ha :: Text -> IO (Maybe Text)
ha p = do
  abilities' <- abilities p
  let (ha', na') = partition snd abilities'
  case ha' of
    [] -> pure Nothing
    [(a, _)] ->
      -- Need to check that it's not a duplicate. Some Pokemon (especially in
      -- Gen 9) have their NAs also listed as HAs. See also
      -- https://github.com/PokeAPI/pokeapi/issues/907
      if a `notElem` map fst na' then pure $ Just a else pure Nothing
    _ -> terror $ "Multiple hidden abilities found for Pokemon " <> p <> ": " <> tshow ha' <> "."

haSpecies :: Text -> IO [(Text, Maybe Text)]
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
