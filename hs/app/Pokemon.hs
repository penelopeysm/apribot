{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Pokemon
  ( ha,
    randomAbility,
    Game (..),
    randomMoves,
    getEmsNoParents,
    getEmsWithParents,
    EggMoveNoParents (..),
    Parent (..),
    EggMoveWithParents (..),
    getPokemonIdsAndDetails,
    SqlException (..),
  )
where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Trans

newtype SqlException = SqlException Text
  deriving (Show)

instance Exception SqlException

sqlError :: (MonadIO m) => Text -> m a
sqlError = liftIO . throwIO . SqlException

makeName :: Text -> Maybe Text -> Text
makeName name form = case form of
  Just f
    | "Alolan" `T.isPrefixOf` f
        || "Galarian" `T.isPrefixOf` f
        || "Hisuian" `T.isPrefixOf` f
        || "Paldean" `T.isPrefixOf` f ->
        f
  Just f | otherwise -> name <> " (" <> f <> ")"
  Nothing -> name

-- * Hidden abilities (Rewritten to use PostgreSQL backend)

-- | Generate a random ability.
randomAbility :: (MonadIO m) => App m (Text, Text)
randomAbility = do
  abty <- withAppPsqlConn $ \conn ->
    query_
      conn
      [sql|SELECT name, flavor_text
                                         FROM abilities
                                         ORDER BY RANDOM()
                                         LIMIT 1;|]
  case abty of
    [(name, flavorText)] -> pure (name, flavorText)
    _ -> sqlError "randomAbility: could not get random ability from database"

ha :: (MonadIO m) => Text -> App m [(Text, Maybe (Text, Text))]
ha name = do
  dbHits :: [(Text, Maybe Text, Maybe Text, Maybe Text)] <-
    withAppPsqlConn $ \conn ->
      query
        conn
        [sql|SELECT p.name, p.form, a.name, a.flavor_text
             FROM pokemon p LEFT OUTER JOIN abilities a
             ON p.ha_id = a.id
             WHERE p.unique_name ILIKE ?;|]
        (Only $ "%" <> name <> "%")
  mapM
    ( \(pname, pform, aname, atext) -> do
        let fullName = makeName pname pform
        case (aname, atext) of
          (Just nm, Just tx) -> pure (fullName, Just (nm, tx))
          (Nothing, Nothing) -> pure (fullName, Nothing)
          _ -> sqlError $ "Invalid database entry for hidden ability " <> name <> ": " <> T.pack (show (aname, atext))
    )
    dbHits

-- * Egg moves

data Parent
  = LevelUpParent {lupPkmnName :: Text, lupLevel :: Int}
  | EvolutionParent {epPkmnName :: Text}
  | BreedParent {bpPkmnName :: Text}
  deriving (Eq, Ord, Show, Generic)

data Game = USUM | SwSh | BDSP | SV deriving (Eq, Ord, Show)

data EggMoveNoParents = EggMoveNoParents
  { emnpName :: Text,
    emnpFlavorText :: Text
  }
  deriving (Eq, Ord, Show)

data EggMoveWithParents = EggMoveWithParents
  { emwpName :: Text,
    emwpFlavorText :: Text,
    emwpParents :: [Parent]
  }
  deriving (Eq, Ord, Show)

-- | Generate a random set of egg moves.
randomMoves :: (MonadIO m) => App m [(Text, Text)]
randomMoves = do
  nMoves :: Int <- liftIO $ randomRIO (2, 6)
  withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT name, flavor_text
             FROM moves
             ORDER BY RANDOM()
             LIMIT ?;|]
      (Only nMoves)

-- TODO: We should fix the special cases in the database
getPokemonIdsAndDetails :: (MonadIO m) => Text -> App m [(Int, Text, Maybe Text, Text)]
getPokemonIdsAndDetails name = do
  let fixedName = case name of
        "morpeko" -> "morpeko-full-belly"
        "eiscue" -> "eiscue-ice"
        t -> t
  withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT id, name, form, unique_name
             FROM pokemon
             WHERE unique_name ILIKE ?;|]
      (Only fixedName)

getEmsNoParents :: (MonadIO m) => Game -> Int -> App m [EggMoveNoParents]
getEmsNoParents game pkmnId = do
  movesAndFlavorTexts :: [(Text, Text)] <-
    withAppPsqlConn $ \conn ->
      query
        conn
        [sql|SELECT m.name, m.flavor_text FROM learnsets as l
                   LEFT JOIN moves as m ON l.move_id = m.id
                   LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                   LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                   LEFT JOIN games as g ON l.game_id = g.id
                   WHERE p.id = ? AND lm.name = 'Egg' AND g.name = ?;|]
        (pkmnId, T.pack (show game))
  pure $ map (uncurry EggMoveNoParents) movesAndFlavorTexts

-- TODO: Need to fix for genderless & female-only Pokemon, as they can pass
-- their own egg moves down to themselves
-- Suggestion: put an extra column in the Pokemon table that says which family a
-- mon belongs to
getParentsGen78 :: (MonadIO m) => [Text] -> Maybe Int -> Text -> Game -> App m [Parent]
getParentsGen78 eggGroups evoFamilyId moveName game = do
  learnParents :: [Parent] <-
    withAppPsqlConn $ \conn ->
      map
        ( \(n, f, maybel) -> case maybel of
            Just l -> LevelUpParent (makeName n f) l
            Nothing -> EvolutionParent (makeName n f)
        )
        <$> query
          conn
          [sql|SELECT p.name, p.form, l.level FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND (lm.name = 'Level up' OR lm.name = 'Evolution')
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND ((p.evolution_family_id IS NOT NULL AND p.evolution_family_id = ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name in ? OR eg2.name in ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
          (moveName, show game, evoFamilyId, In eggGroups, In eggGroups)
  breedParents :: [Parent] <-
    withAppPsqlConn $ \conn ->
      map (\(n, f) -> BreedParent (makeName n f))
        <$> query
          conn
          [sql|SELECT p.name, p.form FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND lm.name = 'Egg'
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND ((p.evolution_family_id IS NOT NULL AND p.evolution_family_id = ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name in ? OR eg2.name in ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
          (moveName, show game, evoFamilyId, In eggGroups, In eggGroups)
  pure $ learnParents <> breedParents

getParentsGen9 :: (MonadIO m) => Text -> Game -> App m [Parent]
getParentsGen9 moveName game = do
  learnParents :: [Parent] <-
    withAppPsqlConn $ \conn ->
      map
        ( \(n, f, maybel) -> case maybel of
            Just l -> LevelUpParent (makeName n f) l
            Nothing -> EvolutionParent (makeName n f)
        )
        <$> query
          conn
          [sql|SELECT p.name, p.form, l.level FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND (lm.name = 'Level up' OR lm.name = 'Evolution')
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
          (moveName, show game)
  breedParents :: [Parent] <-
    withAppPsqlConn $ \conn ->
      map (\(n, f) -> BreedParent (makeName n f))
        <$> query
          conn
          [sql|SELECT p.name, p.form FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND lm.name = 'Egg'
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
          (moveName, show game)
  pure $ learnParents <> breedParents

getEmsWithParents :: (MonadIO m) => Game -> Int -> App m [EggMoveWithParents]
getEmsWithParents game pkmnId = do
  movesAndFlavorTexts :: [(Text, Text)] <-
    withAppPsqlConn $ \conn ->
      query
        conn
        [sql|SELECT m.name, m.flavor_text FROM learnsets as l
                   LEFT JOIN moves as m ON l.move_id = m.id
                   LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                   LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                   LEFT JOIN games as g ON l.game_id = g.id
                   WHERE p.id = ? AND lm.name = 'Egg' AND g.name = ?;|]
        (pkmnId, T.pack (show game))
  forM movesAndFlavorTexts $ \(nm, ft) -> do
    parents <-
      if game `elem` [USUM, SwSh, BDSP]
        then do
          eggGroups <- do
            (eg1, eg2) <- withAppPsqlConn $ \conn ->
              head
                <$> query
                  conn
                  [sql|SELECT eg1.name, eg2.name FROM pokemon as p
                               LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                               LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                               WHERE p.id = ?;|]
                  (Only pkmnId)
            case eg2 of
              Just eg2' -> pure [eg1, eg2']
              Nothing -> pure [eg1]
          familyId <- withAppPsqlConn $ \conn ->
            fromOnly . head
              <$> query
                conn
                [sql|SELECT evolution_family_id FROM pokemon WHERE id = ?;|]
                (Only pkmnId)
          getParentsGen78 eggGroups familyId nm game
        else getParentsGen9 nm game
    pure $ EggMoveWithParents nm ft parents
