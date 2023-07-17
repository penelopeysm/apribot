module Main where

import Config
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import DiscordBot
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
  result <- T.pack <$> readProcess (pythonClassifier config) [] (T.unpack $ postTitle post <> " " <> postBody post)
  case T.strip result of
    "True" -> pure True
    "False" -> pure False
    _ -> do
      T.hPutStrLn stderr ("Invalid result from classifier: <" <> result <> ">")
      pure False

-- | Process newly seen posts.
process :: (MVar (), MVar Post) -> Post -> RedditT (MVar (), MVar Post)
process (stdoutLock, discordLock) post = do
  hit <- liftIO $ isHit post
  if hit
    then do
      liftIO $ do
        addToDb post True
        notifyDiscord discordLock post
        atomically stdoutLock $
          printf "Hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
    else do
      liftIO $ do
        addToDb post False
        atomically stdoutLock $
          printf "Non-hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
  pure (stdoutLock, discordLock)

-- | Thread to stream Reddit posts and process them
redditBot :: MVar () -> MVar Post -> IO ()
redditBot stdoutLock discordLock = do
  atomically stdoutLock $ T.putStrLn "Starting Reddit bot..."
  ownerUsername <- getEnvAsText "REDDIT_USERNAME"
  ownerPassword <- getEnvAsText "REDDIT_PASSWORD"
  ownerClientId <- getEnvAsText "REDDIT_ID"
  ownerClientSecret <- getEnvAsText "REDDIT_SECRET"
  let creds = OwnerCredentials {..}
  env <- authenticate creds (userAgent config)
  let loop = do
        catch
          (runRedditT' env $ postStream defaultStreamSettings process (stdoutLock, discordLock) (watchedSubreddit config))
          ( \(e :: SomeException) -> do
              atomically stdoutLock $ T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              loop
          )
  loop

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- Run Reddit bot, Discord bot, and web server concurrently
  stdoutLock :: MVar () <- newMVar ()
  discordLock :: MVar Post <- newEmptyMVar
  _ <- forkIO $ web stdoutLock
  _ <- forkIO $ redditBot stdoutLock discordLock
  discordBot stdoutLock discordLock
