module RedditBot (redditBot) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (catch)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import Database.SQLite.Simple (withConnection)
import DiscordBot (notifyDiscord)
import Network.HTTP.Req (HttpException)
import Reddit
import System.IO (stderr)
import System.Process (readProcess)
import Text.Printf (printf)
import Trans

-- | Determine whether a post is a hit. This uses an external Python script and
-- a pickled (technically joblib'd) scikit-learn classifier, namely, a stacked
-- ensemble of logistic regression and XGBoost sub-classifiers.
isHit :: FilePath -> Post -> IO Bool
isHit classifierPath post = do
  result <- T.pack <$> readProcess classifierPath [] (T.unpack $ postTitle post <> " " <> postBody post)
  case T.strip result of
    "True" -> pure True
    "False" -> pure False
    _ -> do
      T.hPutStrLn stderr ("Invalid result from classifier: <" <> result <> ">")
      pure False

-- | Process newly seen posts.
process :: Config -> Post -> RedditT Config
process cfg post = do
  let lock = cfgLock cfg
      chan = cfgChan cfg
      postsSql = cfgPostsDbPath cfg
      classifierPath = cfgClassifierPath cfg

  case T.toLower (postSubreddit post) of
    "pokemontrades" -> do
      hit <- liftIO $ isHit classifierPath post
      if hit
        then do
          liftIO $ do
            atomicallyWith lock $
              printf "PTR Hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
            withConnection postsSql $ addToDb post True
            notifyDiscord chan post
        else do
          liftIO $ do
            atomicallyWith lock $
              printf "PTR Non-hit: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
            withConnection postsSql $ addToDb post False
    "bankballexchange" -> do
      liftIO $ do
        atomicallyWith lock $
          printf "BBE: %s\n%s\n%s\n" (unPostID (postId post)) (postTitle post) (postUrl post)
        notifyDiscord chan post
    _ -> pure ()
  pure cfg

-- | Fetch posts from pokemontrades and BankBallExchange.
fetchPosts :: RedditT [Post]
fetchPosts = do
  env <- ask
  let ptr = runRedditT' env $ subredditPosts 50 "pokemontrades" New
      bbe = runRedditT' env $ subredditPosts 50 "BankBallExchange" New
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
  lock <- asks cfgLock
  let creds = OwnerCredentials {..}

  redditEnv <- liftIO $ authenticate creds userAgent
  let settings = defaultStreamSettings {streamsDelay = 10, streamsStorageSize = 400}
  let loop = do
        catch
          (runRedditT' redditEnv $ stream settings process cfg fetchPosts)
          ( \(e :: HttpException) -> do
              atomicallyWith lock $ T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              loop
          )
  liftIO loop
