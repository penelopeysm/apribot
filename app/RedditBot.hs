module RedditBot (redditBot) where

import Control.Concurrent.Async (concurrently)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import Database.SQLite.Simple (withConnection)
import DiscordBot (notifyDiscord)
import Reddit
import System.Process (readProcess)
import Text.Printf (printf)
import Trans

-- | Determine whether a post is a hit. This uses an external Python script and
-- a pickled (technically joblib'd) scikit-learn classifier, namely, a stacked
-- ensemble of logistic regression and XGBoost sub-classifiers.
isHit :: Post -> App IO Bool
isHit post = do
  classifierPath <- asks cfgClassifierPath
  result <- liftIO $ T.pack <$> readProcess classifierPath [] (T.unpack $ postTitle post <> " " <> postBody post)
  case T.strip result of
    "True" -> pure True
    "False" -> pure False
    _ -> do
      atomically $ T.putStrLn ("Invalid result from classifier: <" <> result <> ">")
      pure False

-- | Process newly seen posts.
process :: Post -> RedditT (App IO) ()
process post = do
  cfg <- lift ask
  let chan = cfgChan cfg
      postsSql = cfgPostsDbPath cfg

  case T.toLower (postSubreddit post) of
    "pokemontrades" -> do
      hit <- lift $ isHit post
      if hit
        then do
          lift $ atomically $ printf "PTR Hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
          liftIO $ do
            withConnection postsSql $ addToDb post True
            notifyDiscord chan post
        else do
          lift $ atomically $ printf "PTR Non-hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
          liftIO $ do
            withConnection postsSql $ addToDb post False
    "bankballexchange" -> do
      lift $
        atomically $
          printf "BBE: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
      liftIO $ do
        notifyDiscord chan post
    _ -> pure ()

-- | Fetch posts from pokemontrades and BankBallExchange.
fetchPosts :: RedditT (App IO) [Post]
fetchPosts = do
  env <- ask
  let ptr = runRedditT env $ subredditPosts 50 "pokemontrades" New
      bbe = runRedditT env $ subredditPosts 50 "BankBallExchange" New
  (ptrPosts, bbePosts) <- liftIO $ concurrently ptr bbe
  pure $ ptrPosts <> bbePosts

-- | Thread to stream Reddit posts and process them
redditBot :: App IO ()
redditBot = do
  atomically $ T.putStrLn "Starting Reddit bot..."

  cfg <- ask
  ownerUsername <- asks cfgRedditUsername
  ownerPassword <- asks cfgRedditPassword
  ownerClientId <- asks cfgRedditId
  ownerClientSecret <- asks cfgRedditSecret
  userAgent <- asks cfgUserAgent
  let creds = OwnerCredentials {..}

  redditEnv <- liftIO $ authenticate creds userAgent
  let settings = defaultStreamSettings {streamsDelay = 10, streamsStorageSize = 400}
  runRedditT redditEnv $ stream' settings process (runAppWith cfg) fetchPosts
