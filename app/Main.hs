{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
import Paths_apribot (getDataFileName)
import Reddit
import System.Environment (getEnv)
import System.IO (stderr)
import Text.Printf (printf)

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

-- | Very basic check on whether a string is found in either the
-- post title or the post body.
hasWord :: Post -> Text -> Bool
hasWord p term =
  term `T.isInfixOf` T.toCaseFold (postTitle p)
    || term `T.isInfixOf` T.toCaseFold (postBody p)

-- | Keywords triggering post reporting
keywords :: [Text]
keywords = ["apri", "dream", "beast", "safari", "sport", "fast", "friend", "heavy", "level", "love", "lure", "moon"]

-- | Process newly seen posts
process :: Post -> RedditT ()
process p = do
  let target = PostID "137us03"
  let body =
        printf
          "**Title:** [%s](%s)\n\n**Poster:** [\\/u\\/%s](https://reddit.com/user/%s)\n\n**Tag:** %s\n\n**Posted at:** %s"
          (postTitle p)
          (postUrl p)
          (postAuthor p)
          (postAuthor p)
          (fromMaybe "None" (postFlairText p))
          (show $ postCreatedTime p)
  if any (hasWord p) keywords
    then do
      addNewComment target (T.pack body)
      liftIO $ addToDb p True
      liftIO $ T.putStrLn $ "Notifying about post " <> unPostID (postId p) <> "\n" <> postTitle p <> "\n" <> postUrl p <> "\n"
    else do
      liftIO $ T.putStrLn $ "Found non-matching post\n" <> postTitle p <> "\n" <> postUrl p <> "\n"
      liftIO $ addToDb p False

-- | Add a post to the SQLite database. The Bool parameter indicates whether it
-- was a hit or not.
addToDb :: Post -> Bool -> IO ()
addToDb p isHit = do
  sql <- getDataFileName "/data/test.db" >>= open
  execute_ sql "CREATE TABLE IF NOT EXISTS posts (id TEXT PRIMARY KEY, url TEXT, title TEXT, submitter TEXT, isHit INTEGER)"
  executeNamed
    sql
    "INSERT INTO posts (id, url, title, submitter, isHit) VALUES (:id, :url, :title, :submitter, :isHit)"
    [ ":id" := unPostID (postId p),
      ":url" := postUrl p,
      ":title" := postTitle p,
      ":submitter" := postAuthor p,
      ":isHit" := (if isHit then 1 else 0 :: Integer)
    ]
  close sql

main :: IO ()
main = do
  credsUsername <- getEnvAsText "REDDIT_USERNAME"
  credsPassword <- getEnvAsText "REDDIT_PASSWORD"
  credsClientId <- getEnvAsText "REDDIT_ID"
  credsClientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "github:penelopeysm/apribot by /u/is_a_togekiss"
  let creds = Credentials {..}
  env <- authenticate creds userAgent

  let protected = do
        catch
          (runRedditT' env $ postStream defaultStreamSettings (const process) () "pokemontrades")
          ( \(e :: SomeException) -> do
              T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              protected
          )
  protected
