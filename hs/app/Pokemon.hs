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
    getLegality,
    GenLegality (..),
  )
where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
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
  withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT id, name, form, unique_name
             FROM pokemon
             WHERE unique_name ILIKE ?;|]
      (Only name)

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

-- * Legality

data GenLegality = GenLegality
  { beast :: Bool,
    dream :: Bool,
    apri :: Bool,
    safari :: Bool,
    sport :: Bool
  }
  deriving (Eq, Show)

data Legality = Legality
  { bank :: GenLegality,
    home :: GenLegality
  }
  deriving (Eq, Show)

legalityOr :: Legality -> Legality -> Legality
legalityOr (Legality b1 h1) (Legality b2 h2) =
  Legality
    (GenLegality (beast b1 || beast b2) (dream b1 || dream b2) (apri b1 || apri b2) (safari b1 || safari b2) (sport b1 || sport b2))
    (GenLegality (beast h1 || beast h2) (dream h1 || dream h2) (apri h1 || apri h2) (safari h1 || safari h2) (sport h1 || sport h2))

genLegalityAndBool :: GenLegality -> Bool -> GenLegality
genLegalityAndBool (GenLegality b d a s sp) singleBool =
  GenLegality (b && singleBool) (d && singleBool) (a && singleBool) (s && singleBool) (sp && singleBool)

legalityAllFalse :: Legality
legalityAllFalse = Legality (GenLegality False False False False False) (GenLegality False False False False False)

-- | Parse the results of a SQL query into a 'Legality'. Make sure the columns
-- requested are in the correct order!
parseLegalitySql :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Legality
parseLegalitySql (b1, d1, a1, s1, sp1, b2, d2, a2, s2, sp2) =
  Legality
    (GenLegality b1 d1 a1 s1 sp1)
    (GenLegality b2 d2 a2 s2 sp2)

getLegality :: (MonadIO m) => Int -> App m (Map Game (Bool, GenLegality))
getLegality pkmnId = do
  -- Get the legality of all members in the same evolution family, and combine
  -- them using `legalityOr`. In practice, this accomplishes two things:
  -- 1. We can search for legality of evolutions.
  -- 2. We can combine legality of regional variants and other forms, assuming
  -- that they are crossbreedable. The only exceptions to this, as of SV DLC1,
  -- are Tauros and Flabebe. This is taken care of in the database by:
  --    - Not having the different Tauros forms belong to the same evolution
  --      family. (Their evolution family is NULL)
  --    - Not listing separate Flabebe forms.
  evoFamilyId :: Maybe Int <- do
    sqlEvoFamilyId <- withAppPsqlConn $ \conn ->
      query
        conn
        [sql|SELECT evolution_family_id FROM pokemon WHERE id = ?;|]
        (Only pkmnId)
    case sqlEvoFamilyId of
      [Only i] -> pure i
      _ -> error "getLegality: expected exactly one evolution family id"
  legalitySql :: [Legality] <- withAppPsqlConn $ \conn ->
    map parseLegalitySql
      <$> case evoFamilyId of
        Just efId ->
          query
            conn
            [sql|SELECT l.bank_beast, l.bank_dream, l.bank_apri, l.bank_safari, l.bank_sport,
                            l.home_beast, l.home_dream, l.home_apri, l.home_safari, l.home_sport
                            FROM legality as l
                            LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                            WHERE p.evolution_family_id = ?;|]
            (Only efId)
        Nothing ->
          query
            conn
            [sql|SELECT l.bank_beast, l.bank_dream, l.bank_apri, l.bank_safari, l.bank_sport,
                            l.home_beast, l.home_dream, l.home_apri, l.home_safari, l.home_sport
                            FROM legality as l
                            LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                            WHERE p.id = ?;|]
            (Only pkmnId)
  let baseLegality = foldl' legalityOr legalityAllFalse legalitySql

  -- `baseLegality` tells us the legality of a Pokemon in Bank and HOME.
  -- However, it does not take care of the case where a Pokemon cannot enter a
  -- game (due to Dexit). To figure this out, we need to determine which mons
  -- can enter which game. This is done by querying the learnsets table: if a
  -- mon cannot learn any move in a game, we assume this means it cannot enter
  -- the game.
  gamesAvailableIn <- do
    withAppPsqlConn $ \conn -> do
      gameIds <-
        query
          conn
          [sql|SELECT g.id
                 FROM learnsets as l
                 INNER JOIN games as g ON l.game_id = g.id
                 WHERE l.pokemon_id = ?
                 GROUP BY g.id;|]
          (Only pkmnId)
      pure $ map (\case Only (1 :: Int) -> USUM; Only 2 -> SwSh; Only 3 -> BDSP; Only 4 -> SV; _ -> error "getLegality: unexpected game") gameIds
  pure $
    M.fromList
      [ (USUM, (USUM `elem` gamesAvailableIn, bank baseLegality `genLegalityAndBool` (USUM `elem` gamesAvailableIn))),
        (SwSh, (SwSh `elem` gamesAvailableIn, home baseLegality `genLegalityAndBool` (SwSh `elem` gamesAvailableIn))),
        (BDSP, (BDSP `elem` gamesAvailableIn, home baseLegality `genLegalityAndBool` (BDSP `elem` gamesAvailableIn))),
        (SV, (SV `elem` gamesAvailableIn, home baseLegality `genLegalityAndBool` (SV `elem` gamesAvailableIn)))
      ]
