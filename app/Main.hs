module Main where

import Config
import Control.Concurrent (MVar, forkIO, newMVar, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import Reddit
import System.IO
import System.Process (readProcess)
import Text.Printf (printf)
import Utils
import Web

-- | Determine whether a post is a hit. This uses an external Python script and
-- a pickled (technically joblib'd) scikit-learn classifier, namely, a stacked
-- ensemble of logistic regression and XGBoost sub-classifiers.
isHit :: Post -> IO Bool
isHit post = do
  result <- readProcess (pythonClassifier config) [] (T.unpack $ postTitle post <> " " <> postBody post)
  case result of
    "True" -> pure True
    "False" -> pure False
    _ -> do
      T.hPutStrLn stderr ("Invalid result from classifier: " <> T.pack result)
      pure False

-- | Process newly seen posts.
process :: MVar () -> Post -> RedditT (MVar ())
process lock post = do
  let commentText =
        printf
          "**Title:** [%s](%s)\n\n**Poster:** [\\/u\\/%s](https://reddit.com/user/%s)\n\n**Tag:** %s\n\n**Posted at:** %s"
          (postTitle post)
          (postUrl post)
          (markdownEscape $ postAuthor post)
          (postAuthor post)
          (fromMaybe "None" (postFlairText post))
          (show $ postCreatedTime post)
  hit <- liftIO $ isHit post
  if hit
    then do
      addNewComment (notifyOnPostId config) (T.pack commentText)
      liftIO $ do
        addToDb post True
        atomically lock $
          printf "Hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
    else do
      liftIO $ do
        addToDb post False
        atomically lock $
          printf "Non-hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
  pure lock

-- | Thread to stream Reddit posts and process them
bot :: MVar () -> IO ()
bot lock = do
  atomically lock $ T.putStrLn "Starting Reddit bot..."
  ownerUsername <- getEnvAsText "REDDIT_USERNAME"
  ownerPassword <- getEnvAsText "REDDIT_PASSWORD"
  ownerClientId <- getEnvAsText "REDDIT_ID"
  ownerClientSecret <- getEnvAsText "REDDIT_SECRET"
  let creds = OwnerCredentials {..}
  env <- authenticate creds (userAgent config)
  let loop = do
        catch
          (runRedditT' env $ postStream defaultStreamSettings process lock (watchedSubreddit config))
          ( \(e :: SomeException) -> do
              atomically lock $ T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              loop
          )
  loop

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- Run bot and web server concurrently
  lock <- newMVar ()
  _ <- forkIO $ web lock
  bot lock
