{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Pokemon
  ( SqlException (..),
    getLegality,
    GenLegality (..),
    SuggestedNature (..),
    getSuggestedNatures,
    getSprites,
  )
where

-- import Apripsql.Queries (DBPokemon (..))
import qualified Apripsql.Queries as Q
import Control.Exception (Exception (..), SomeException, try)
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
import Image (hstackPngs, vstackPng)
import Network.HTTP.Req
import Setup.Game (Game (..)) -- from apripsql
import Trans

-- * General helpers

newtype SqlException = SqlException Text
  deriving (Show)

instance Exception SqlException

-- * Hidden abilities

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
  -- them using `legalityOr`
  evoTreeMembers <- withAppPsqlConn $ Q.getAllEvolutionTreeMembers pkmnId
  atomically $ print evoTreeMembers
  crossbreedableEvoTreeMembers <- withAppPsqlConn $ Q.getAllCrossbreedableForms evoTreeMembers
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

getSuggestedNatures :: (MonadIO m) => Int -> App m (Maybe SuggestedNature)
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
