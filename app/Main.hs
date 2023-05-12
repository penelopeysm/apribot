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

-- | Post a comment about the post on a thread
notify :: Post -> RedditT ()
notify p = do
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
      liftIO $ T.putStrLn $ "Notifying about post " <> unPostID (postId p) <> "\n" <> postTitle p <> "\n" <> postUrl p <> "\n"
    else liftIO $ T.putStrLn $ "Found non-matching post\n" <> postTitle p <> "\n" <> postUrl p <> "\n"

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
          (runRedditT' env $ postStream defaultStreamSettings (const notify) () "pokemontrades")
          ( \(e :: SomeException) -> do
              T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              protected
          )
  protected
