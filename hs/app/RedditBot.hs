module RedditBot (redditBot) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, catch)
import Data.Text (Text)
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

-- | Action to fetch posts from a subreddit
fetchPosts :: Int -> Text -> RedditT (App IO) [Post]
fetchPosts n subreddit = do
  env <- ask
  lift $ atomically $ T.putStrLn $ "Fetching posts from r/" <> subreddit <> "..."
  runRedditT env $ subredditPosts n subreddit New

-- | Stream posts from a subreddit, but additionally unwrap the monad
-- transformer stack
streamReddit :: Config -> RedditEnv -> Int -> Text -> IO ()
streamReddit cfg env n subreddit = do
  let settings =
        defaultStreamSettings
          { streamsDelay = cfgRedditStreamDelay cfg,
            streamsStorageSize = 2 * n
          }
      -- Unwind monad transformers. :upside_down_smile:
      unwrapApp = runAppWith cfg
      unwrapReddit = runRedditT env
   in unwrapApp . unwrapReddit $ stream' settings (lift . process) unwrapApp (fetchPosts n subreddit)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Restart an IO action on exception (and ping me on Discord with the
-- exception)
protect :: Config -> IO () -> IO ()
protect cfg action =
  action
    `catch` ( \(e :: SomeException) -> do
                let errMsg = "<@" <> tshow (cfgPennyId cfg) <> "> Exception: " <> tshow e
                runAppWith cfg $ notifyDiscord (Log errMsg)
                protect cfg action
            )

-- | Thread to stream Reddit posts and process them
redditBot :: App IO ()
redditBot = do
  atomically $ T.putStrLn "Starting Reddit bot..."
  cfg <- ask
  redditEnv <- authenticateAsOwner
  void $
    liftIO $
      concurrently
        (protect $ streamReddit cfg redditEnv 50 "pokemontrades")
        (protect $ streamReddit cfg redditEnv 5 "bankballexchange")
