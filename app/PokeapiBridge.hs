{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
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
    getPokemonIdsAndDetails,
    MyException (..),
  )
where

import Control.Exception (Exception (..), throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Trans

data MyException
  = SqlException Text
  deriving (Show)

instance Exception MyException

sqlError :: (MonadIO m) => Text -> m a
sqlError = liftIO . throwIO . SqlException

-- * Hidden abilities (Rewritten to use PostgreSQL backend)

-- | Generate a random ability.
randomAbility :: (MonadIO m) => App m (Text, Text)
randomAbility = do
  cfg <- ask
  conn <- liftIO $ cfgPsqlConn cfg
  abty <-
    liftIO $
      PSQL.query_
        conn
        [sql|SELECT name, flavor_text
                                         FROM abilities
                                         ORDER BY RANDOM()
                                         LIMIT 1;|]
  liftIO $ PSQL.close conn
  case abty of
    [(name, flavorText)] -> pure (name, flavorText)
    _ -> sqlError "randomAbility: could not get random ability from database"

ha :: (MonadIO m) => Text -> App m [(Text, Maybe (Text, Text))]
ha name = do
  cfg <- ask
  conn <- liftIO $ cfgPsqlConn cfg
  dbHits :: [(Text, Maybe Text, Maybe Text, Maybe Text)] <-
    liftIO $
      PSQL.query
        conn
        [sql|SELECT p.name, p.form, a.name, a.flavor_text
             FROM pokemon p LEFT OUTER JOIN abilities a
             ON p.ha_id = a.id
             WHERE p.unique_name ILIKE ?;|]
        (PSQL.Only $ "%" <> name <> "%")
  liftIO $ PSQL.close conn
  mapM
    ( \(pname, pform, aname, atext) -> do
        let fullName = case pform of
              Just form -> pname <> " (" <> form <> ")"
              Nothing -> pname
        case (aname, atext) of
          (Just nm, Just tx) -> pure (fullName, Just (nm, tx))
          (Nothing, Nothing) -> pure (fullName, Nothing)
          _ -> sqlError $ "Invalid database entry for hidden ability " <> name <> ": " <> T.pack (show (aname, atext))
    )
    dbHits

-- * Egg moves

data Parent
  = LevelUpParent {lupPkmnName :: Text, lupOrder :: Int, lupLevel :: Int}
  | BreedParent {bpPkmnName :: Text, bpOrder :: Int}
  | BothParent {bothPkmnName :: Text, bothOrder :: Int, bothLevel :: Int}
  deriving (Eq, Ord, Show, Generic)

order :: Parent -> Int
order (LevelUpParent _ o _) = o
order (BreedParent _ o) = o
order (BothParent _ o _) = o

data Game = USUM | SwSh | BDSP | SV deriving (Eq, Ord, Show)

data EggMove = EggMove
  { emName :: Text,
    emFlavorText :: Text,
    emParents :: [Parent]
  }
  deriving (Eq, Ord, Show)

-- | Generate a random set of egg moves.
randomMoves :: (MonadIO m) => App m [(Text, Text)]
randomMoves = do
  cfg <- ask
  conn <- liftIO $ cfgPsqlConn cfg
  nMoves :: Int <- liftIO $ randomRIO (2, 6)
  moves <-
    liftIO $
      PSQL.query
        conn
        [sql|SELECT name, flavor_text
                                         FROM moves
                                         ORDER BY RANDOM()
                                         LIMIT ?;|]
        (PSQL.Only nMoves)
  liftIO $ PSQL.close conn
  pure moves

-- TODO: We should fix the special cases in the database
getPokemonIdsAndDetails :: (MonadIO m) => Text -> App m [(Int, Text, Maybe Text, Text)]
getPokemonIdsAndDetails name = do
  let fixedName = case name of
        "morpeko" -> "morpeko-full-belly"
        "eiscue" -> "eiscue-ice"
        t -> t
  cfg <- ask
  conn <- liftIO $ cfgPsqlConn cfg
  exactMatches :: [(Int, Text, Maybe Text, Text)] <-
    liftIO $
      PSQL.query
        conn
        [sql|SELECT id, name, form, unique_name
             FROM pokemon
             WHERE unique_name ILIKE ?;|]
        (PSQL.Only fixedName)
  liftIO $ PSQL.close conn
  pure exactMatches

-- TODO: Get parents
em :: (MonadIO m) => Game -> Int -> App m [EggMove]
em game pkmnId = do
  cfg <- ask
  conn <- liftIO $ cfgPsqlConn cfg
  movesAndFlavorTexts :: [(Text, Text)] <-
    liftIO $
      PSQL.query
        conn
        [sql|SELECT m.name, m.flavor_text FROM learnsets as l
                   LEFT JOIN moves as m ON l.move_id = m.id
                   LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                   LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                   LEFT JOIN games as g ON l.game_id = g.id
                   WHERE p.id = ? AND lm.name = 'Egg' AND g.name = ?;|]
        (pkmnId, T.pack (show game))
  liftIO $ PSQL.close conn
  pure $
    map
      ( \(moveName, flavorText) ->
          EggMove
            { emName = moveName,
              emFlavorText = flavorText,
              emParents = []
            }
      )
      movesAndFlavorTexts
