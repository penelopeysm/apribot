module RedditBot (redditBot) where

import Config
import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.Chan (Chan)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import DiscordBot (notifyDiscord)
import Reddit
import System.IO (stderr)
import System.Process (readProcess)
import Text.Printf (printf)
import Utils

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
process :: (MVar (), Chan Post) -> Post -> RedditT (MVar (), Chan Post)
process (stdoutLock, discordChan) post = do
  case T.toLower (postSubreddit post) of
    "pokemontrades" -> do
      hit <- liftIO $ isHit post
      if hit
        then do
          liftIO $ do
            addToDb post True
            notifyDiscord discordChan post
            atomically stdoutLock $
              printf "PTR Hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
        else do
          liftIO $ do
            addToDb post False
            atomically stdoutLock $
              printf "PTR Non-hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
    "bankballexchange" -> do
      liftIO $ do
        notifyDiscord discordChan post
        atomically stdoutLock $
          printf "BBE: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
    _ -> pure ()
  pure (stdoutLock, discordChan)

-- | Fetch posts from pokemontrades and BankBallExchange.
fetchPosts :: RedditT [Post]
fetchPosts = do
  ptr <- subredditPosts 50 "pokemontrades" New
  bbe <- subredditPosts 50 "BankBallExchange" New
  pure $ ptr <> bbe

-- | Thread to stream Reddit posts and process them
redditBot :: MVar () -> Chan Post -> IO ()
redditBot stdoutLock discordLock = do
  atomically stdoutLock $ T.putStrLn "Starting Reddit bot..."
  ownerUsername <- getEnvAsText "REDDIT_USERNAME"
  ownerPassword <- getEnvAsText "REDDIT_PASSWORD"
  ownerClientId <- getEnvAsText "REDDIT_ID"
  ownerClientSecret <- getEnvAsText "REDDIT_SECRET"
  let creds = OwnerCredentials {..}
  env <- authenticate creds (userAgent config)
  let settings = defaultStreamSettings {streamsDelay = 10, streamsStorageSize = 400}
  let loop = do
        catch
          (runRedditT' env $ stream settings process (stdoutLock, discordLock) fetchPosts)
          ( \(e :: SomeException) -> do
              atomically stdoutLock $ T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              loop
          )
  loop
