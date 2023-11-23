{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Pokemon
  ( ha,
    getAbilityName,
    randomAbility,
    SqlPkmn (..),
    Game (..),
    randomMoves,
    getEmsNoParents,
    getEmsWithParents,
    EggMoveNoParents (..),
    Parent (..),
    EggMoveWithParents (..),
    getPokemonIdsAndDetails,
    GetPokemonIdResult (..),
    SqlException (..),
    getLegality,
    GenLegality (..),
    isPokemonUnbreedable,
    SuggestedNature (..),
    getSuggestedNatures,
    getSprites,
  )
where

import Control.Exception (Exception (..), SomeException, throwIO, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import Image (hstackPngs, vstackPng)
import Network.HTTP.Req
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

-- * General helpers

type PkmnId = Int

data SqlPkmn = SqlPkmn
  { spId :: PkmnId,
    spName :: Text,
    spForm :: Maybe Text,
    spUniqueName :: Text,
    spNdex :: Int,
    spGalarDex :: Maybe Int, -- Galar
    spIoaDex :: Maybe Int, -- Isle of Armor
    spCtDex :: Maybe Int, -- Crown Tundra
    spPaldeaDex :: Maybe Int, -- Paldea
    spTmDex :: Maybe Int, -- Teal Mask
    spType1Id :: Int,
    spType2Id :: Maybe Int,
    spHp :: Int,
    spAtk :: Int,
    spDef :: Int,
    spSpa :: Int,
    spSpd :: Int,
    spSpe :: Int,
    spEggGroup1Id :: Int,
    spEggGroup2Id :: Maybe Int,
    spGenderRatioId :: Int,
    spAbility1Id :: Int,
    spAbility2Id :: Maybe Int,
    spHiddenAbilityId :: Maybe Int,
    spEggCycles :: Int
  }
  deriving (Eq, Ord, Show, Generic, FromRow)

data GetPokemonIdResult
  = NoneFound
  | NoneFoundButSuggesting [Text]
  | FoundOne SqlPkmn
  deriving (Eq, Ord, Show)

getPokemonIdsAndDetails :: (MonadIO m) => Text -> App m GetPokemonIdResult
getPokemonIdsAndDetails name = do
  let hyphenatedName =
        T.replace "farfetch'd" "farfetchd"
          . T.replace "sirfetch'd" "sirfetchd"
          . T.replace "mr.-mime" "mr-mime"
          . T.replace "mime-jr." "mime-jr"
          . T.replace "mr.-rime" "mr-rime"
          . T.toLower
          . T.intercalate "-"
          . T.words
          $ name
  results <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT id, name, form, unique_name, ndex, galar_dex, ioa_dex, ct_dex, paldea_dex,
                  tm_dex, type1_id, type2_id, hp, atk, def, spa, spd, spe, eg1_id, eg2_id,
                  gr_id, ability1_id, ability2_id, ha_id, egg_cycles
             FROM pokemon
             WHERE unique_name ILIKE ?;|]
      (Only $ hyphenatedName <> "%")
  case results of
    [] -> pure NoneFound
    [x] -> pure $ FoundOne x
    xs -> case filter (\ps -> spUniqueName ps == hyphenatedName) xs of
      [] -> pure $ NoneFoundButSuggesting (spUniqueName <$> xs)
      [x] -> pure $ FoundOne x
      _ -> error "getPokemonIdsAndDetails: multiple results returned"

-- | Get the ID of the base form of a given Pokemon.
_getBaseForm :: (MonadIO m) => PkmnId -> App m PkmnId
_getBaseForm pkmnId = do
  baseForm <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|WITH RECURSIVE cte_base AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE evo_id = ?
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_base c ON c.prevo_id = e.evo_id
      )
      SELECT prevo_id
      FROM (
        -- This gets all real prevos
        SELECT * FROM cte_base
        -- This adds the current form being searched for to the list
        UNION SELECT * FROM (VALUES (?, 0, -1)) AS t (prevo_id, evo_id, level)
        ORDER BY level DESC LIMIT 1
      ) AS t2;|]
      (pkmnId, pkmnId)
  case baseForm of
    [] -> pure pkmnId -- No prevos.
    (Only baseFormId : _) -> pure baseFormId -- Found prevo.

-- | Get all parents of a Pokemon recursively. Note that this function should be
-- called on the base form of an evolution tree.
_getAllParents :: (MonadIO m) => PkmnId -> App m [PkmnId]
_getAllParents pkmnId = do
  evos <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|WITH RECURSIVE evos AS (
          SELECT prevo_id, evo_id FROM evolutions
          WHERE prevo_id = ?
          UNION
          SELECT e.prevo_id, e.evo_id FROM evolutions e
          INNER JOIN evos e2 ON e2.evo_id = e.prevo_id
        )
        SELECT evo_id FROM evos;|]
      (Only pkmnId)
  pure $ pkmnId : map fromOnly evos

-- | Get all members of an evolution tree. In principle, this could be
-- implemented as _getBaseForm >=> _getAllParents, but that would require two
-- database calls. Here we've merged it into one single query. I'm not actually
-- sure if this is faster, though.
getAllEvolutionTreeMembers :: (MonadIO m) => PkmnId -> App m [PkmnId]
getAllEvolutionTreeMembers pkmnId = do
  ns <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|
      WITH RECURSIVE
      cte_base AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE evo_id = ?
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_base c ON c.prevo_id = e.evo_id
      ),
      base_form_id AS (
        SELECT prevo_id
        FROM (SELECT *
            FROM cte_base
            UNION SELECT * FROM (VALUES (?, 0, -1)) AS t (prevo_id, evo_id, level)
            ORDER BY level DESC LIMIT 1)
        AS t2
      ),
      cte_evos AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE prevo_id = (SELECT prevo_id FROM base_form_id)
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_evos c ON c.evo_id = e.prevo_id
      )
      SELECT evo_id FROM cte_evos
      UNION (SELECT prevo_id FROM base_form_id)
      ORDER BY evo_id ASC;|]
      (pkmnId, pkmnId)
  pure $ map fromOnly ns

-- | Get all Pokemon forms which can be crossbred from a given list of Pokemon.
getAllCrossbreedableForms :: (MonadIO m) => [PkmnId] -> App m [PkmnId]
getAllCrossbreedableForms pkmnIds = do
  ns <- withAppPsqlConn $ \conn -> do
    query
      conn
      [sql|
      SELECT p.id
        FROM pokemon as p
       WHERE (p.ndex IN (SELECT DISTINCT ndex FROM pokemon WHERE id IN ?) AND p.gr_id IN (3, 4, 5, 6, 7))
          OR p.id IN ?
      |]
      (In pkmnIds, In pkmnIds)
  pure $ map fromOnly ns

-- | Returns whether a Pokemon is unbreedable. This is true iff all members of
-- the Pokemon's evolution tree belong to unbreedable egg groups.
isPokemonUnbreedable :: (MonadIO m) => PkmnId -> App m Bool
isPokemonUnbreedable pkmnId = do
  evoFamilyIds <- getAllEvolutionTreeMembers pkmnId
  result <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|
      SELECT NOT EXISTS(
          SELECT * FROM pokemon as p2
          WHERE p2.id IN ?
          AND p2.eg1_id NOT IN (13, 15)
      );
      |]
      (Only $ In evoFamilyIds)
  case result of
    [Only True] -> pure True
    [Only False] -> pure False
    _ -> error "isPokemonUnbreedable: expected exactly one Boolean result"

-- * Hidden abilities

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

getAbilityName :: (MonadIO m) => Int -> App m Text
getAbilityName abilityId = do
  result <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT name FROM abilities WHERE id = ?;|]
      (Only abilityId)
  case result of
    [Only name] -> pure name
    _ -> sqlError "getAbilityName: could not get ability name from database"

ha :: (MonadIO m) => PkmnId -> App m (Maybe (Text, Text))
ha pkmnId = do
  result <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT a.name, a.flavor_text
             FROM pokemon p LEFT OUTER JOIN abilities a
             ON p.ha_id = a.id
             WHERE p.id = ?;|]
      (Only pkmnId)
  case result of
    [(Just name, Just flavorText)] -> pure $ Just (name, flavorText)
    [(Nothing, Nothing)] -> pure Nothing
    _ -> error $ "HA query returned invalid results (this should not happen!): " <> show result

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

getEmsNoParents :: (MonadIO m) => Game -> PkmnId -> App m [EggMoveNoParents]
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

getEmsWithParents :: (MonadIO m) => Game -> PkmnId -> App m [EggMoveWithParents]
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

getLegality :: (MonadIO m) => PkmnId -> App m (Map Game (Bool, GenLegality))
getLegality pkmnId = do
  -- Get the legality of all members in the same evolution family, and combine
  -- them using `legalityOr`
  evoTreeMembers <- getAllEvolutionTreeMembers pkmnId
  atomically $ print evoTreeMembers
  crossbreedableEvoTreeMembers <- getAllCrossbreedableForms evoTreeMembers
  atomically $ print crossbreedableEvoTreeMembers
  legalitySql :: [Legality] <- withAppPsqlConn $ \conn ->
    map parseLegalitySql
      <$> query
        conn
        [sql|
        SELECT l.bank_beast, l.bank_dream, l.bank_apri, l.bank_safari, l.bank_sport,
               l.home_beast, l.home_dream, l.home_apri, l.home_safari, l.home_sport
        FROM legality as l
        LEFT JOIN pokemon as p ON l.pokemon_id = p.id
        WHERE p.id IN ?;
        |]
        (Only $ In crossbreedableEvoTreeMembers)
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
        if pkmnId == 371 -- Spinda
          then (BDSP, (True, GenLegality False False True False False))
          else (BDSP, (BDSP `elem` gamesAvailableIn, home baseLegality `genLegalityAndBool` (BDSP `elem` gamesAvailableIn))),
        (SV, (SV `elem` gamesAvailableIn, home baseLegality `genLegalityAndBool` (SV `elem` gamesAvailableIn)))
      ]

-- * Natures

data SuggestedNature = SuggestedNature
  { penny :: Maybe Text,
    jemmaSwSh :: Maybe Text,
    jemmaBDSP :: Maybe Text,
    jemmaG7 :: Maybe Text
  }

getSuggestedNatures :: (MonadIO m) => PkmnId -> App m (Maybe SuggestedNature)
getSuggestedNatures pkmnId = do
  suggestedNatures <- withAppPsqlConn $ \conn ->
    query
      conn
      [sql|SELECT penny, jemma_swsh, jemma_bdsp, jemma_g7 FROM natures WHERE pokemon_id = ?;|]
      (Only pkmnId)
  liftIO $ print suggestedNatures
  case suggestedNatures of
    [] -> pure Nothing
    [(p, js, jb, j7)] -> pure $ Just $ SuggestedNature p js jb j7
    _ -> error "getNatures: got more than one set of suggested natures"

-- * Sprites

getSpriteUrls :: (MonadIO m) => Int -> App m [Text]
getSpriteUrls pkmnNdex = do
  -- This file contains a list of all the filenames in the shiny folder of the
  -- dex_formname_female branch of penelopeysm/RareballSpreadsheet. We use the
  -- shiny folder because the list of files there is a strict subset of the
  -- files in the non-shiny ('original') folder.
  cts <- liftIO $ T.readFile "static/sprite-filenames.txt"
  let pkmnIdT = T.pack $ show pkmnNdex
  let allFileNames = T.lines cts
  pure $ filter (\fname -> fname == pkmnIdT <> ".png" || (pkmnIdT <> "_") `T.isPrefixOf` fname) allFileNames

getOneSprite :: (MonadIO m) => Bool -> Text -> App m (Maybe ByteString)
getOneSprite shiny fname = do
  let dir = if shiny then "shiny" else "original"
  let url = https "raw.githubusercontent.com" /: "penelopeysm" /: "RareballSpreadsheet" /: "dex_formname_female" /: dir /: fname
  (eitherBs :: Either SomeException BsResponse) <-
    liftIO $
      try $
        runReq defaultHttpConfig $
          req GET url NoReqBody bsResponse mempty
  case eitherBs of
    Left err -> atomically (print err) >> pure Nothing
    Right bs -> pure $ Just (responseBody bs)

generateCombinedSprites :: (MonadIO m) => [Text] -> App m (Maybe ByteString)
generateCombinedSprites fnames =
  case fnames of
    [] -> pure Nothing
    _ -> do
      nonshinySprites <- catMaybes <$> mapM (getOneSprite False) fnames
      shinySprites <- catMaybes <$> mapM (getOneSprite True) fnames
      pure $ case zipWith vstackPng nonshinySprites shinySprites of
        [] -> Nothing
        xs -> Just $ hstackPngs xs

getSprites :: (MonadIO m) => Int -> App m (Maybe ByteString)
getSprites pkmnNdex = getSpriteUrls pkmnNdex >>= generateCombinedSprites
