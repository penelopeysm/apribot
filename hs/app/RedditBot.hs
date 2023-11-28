module RedditBot (redditBot) where

import Control.Concurrent.Async (concurrently)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import DiscordBot (notifyDiscord)
import Reddit
import System.Process (readProcess)
import Trans

-- | Determine whether a post is a hit. This uses an external Python script and
-- a pickled (technically joblib'd) scikit-learn classifier, namely, a stacked
-- ensemble of logistic regression and XGBoost sub-classifiers.
isHit :: Post -> App IO HitStatus
isHit post = do
  classifierPath <- asks cfgClassifierPath
  result <- liftIO $ T.pack <$> readProcess classifierPath [] (T.unpack $ postTitle post <> " " <> postBody post)
  case T.strip result of
    "-1" -> pure DefinitelyNegative
    "0" -> pure Negative
    "1" -> pure Positive
    _ -> do
      atomically $ T.putStrLn ("Invalid result from classifier: <" <> result <> ">")
      pure Negative

-- | Process newly seen posts.
process :: Post -> App IO ()
process post = do
  case T.toLower (postSubreddit post) of
    "pokemontrades" -> do
      hit <- isHit post
      let outputStr = case hit of
            DefinitelyNegative -> "Definitely non-hit"
            Negative -> "Non-hit"
            Positive -> "Hit"
      addToDb post hit
      atomically $
        T.putStrLn $
          T.intercalate
            " "
            [ "PTR:",
              outputStr,
              unPostID (postId post),
              postTitle post,
              postUrl post
            ]
      when (hit == Positive) $ notifyDiscord (NotifyPost post)
    "bankballexchange" -> do
      atomically $
        T.putStrLn $
          T.intercalate
            " "
            [ "BBE:",
              unPostID (postId post),
              postTitle post,
              postUrl post
            ]
      notifyDiscord (NotifyPost post)
    _ -> pure ()

-- | Fetch posts from pokemontrades and BankBallExchange.
fetchPosts :: RedditT (App IO) [Post]
fetchPosts = do
  env <- ask
  lift $ atomically $ T.putStrLn "Fetching posts from subreddits..."
  let ptr = runRedditT env $ subredditPosts 50 "pokemontrades" New
      bbe = runRedditT env $ subredditPosts 5 "BankBallExchange" New
  (ptrPosts, bbePosts) <- liftIO $ concurrently ptr bbe
  lift $ notifyDiscord (Log ("BBE posts: " <> T.intercalate ", " (map (unPostID . postId) bbePosts)))
  pure $ ptrPosts <> bbePosts

-- | Thread to stream Reddit posts and process them
redditBot :: App IO ()
redditBot = do
  atomically $ T.putStrLn "Starting Reddit bot..."
  cfg <- ask
  redditEnv <- authenticateAsOwner
  let settings =
        defaultStreamSettings
          { streamsDelay = cfgRedditStreamDelay cfg,
            streamsStorageSize = 400
          }
  runRedditT redditEnv $ stream' settings (lift . process) (runAppWith cfg) fetchPosts
