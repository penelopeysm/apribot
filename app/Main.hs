module Main where

import Config
import Control.Concurrent (MVar, forkIO, newMVar, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database
import Reddit
import System.IO
import Text.Printf (printf)
import Utils
import Web

-- | Checks whether a string is found in either the post title or the post body.
hasWord :: Post -> Text -> Bool
hasWord post term =
  term `T.isInfixOf` T.toCaseFold (postTitle post)
    || term `T.isInfixOf` T.toCaseFold (postBody post)

-- | TODO: Replace with deep learning
isHit :: Post -> Bool
isHit post = any (hasWord post) (keywords config)

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
  if isHit post
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
