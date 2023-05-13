{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (MVar, forkIO, newMVar, threadDelay, withMVar)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import Database.SQLite.Simple
import Paths_apribot (getDataFileName)
import Reddit
import System.Environment (getEnv)
import System.IO
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import qualified Web.Scotty as Scotty

data Config = Config
  { dbFileName :: String,
    watchedSubreddit :: Text,
    notifyOnPostId :: ID Post,
    port :: Int,
    keywords :: [Text],
    userAgent :: Text
  }

-- | App configuration.
config :: Config
config =
  Config
    { dbFileName = "/data/posts.db",
      watchedSubreddit = "pokemontrades",
      notifyOnPostId = PostID "13fzjhf",
      port = 8080,
      keywords = ["apri", "dream", "beast", "safari", "sport", "fast", "friend", "heavy", "level", "love", "lure", "moon"],
      userAgent = "github:penelopeysm/apribot by /u/is_a_togekiss"
    }

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

atomically :: MVar () -> IO () -> IO ()
atomically lock action = withMVar lock $ const action

-- | Checks whether a string is found in either the post title or the post body.
hasWord :: Post -> Text -> Bool
hasWord post term =
  term `T.isInfixOf` T.toCaseFold (postTitle post)
    || term `T.isInfixOf` T.toCaseFold (postBody post)

-- | TODO: Replace with deep learning
isHit :: Post -> Bool
isHit post = any (hasWord post) (keywords config)

-- | In principle, more characters should be escaped. However, the only texts
-- we're escaping (fornow) are Reddit usernames, and I think the underscore is
-- the only character that is both allowed in a username and has a special
-- meaning in Markdown.
markdownEscape :: Text -> Text
markdownEscape = T.replace "_" "\\_"

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

-- | Add a post to the SQLite database. The Bool parameter indicates whether it
-- was a hit or not.
addToDb :: Post -> Bool -> IO ()
addToDb post hit = do
  sql <- getDataFileName (dbFileName config) >>= open
  execute_ sql "CREATE TABLE IF NOT EXISTS posts (id TEXT PRIMARY KEY, url TEXT, title TEXT, submitter TEXT, isHit INTEGER, time TEXT, flair TEXT)"
  executeNamed
    sql
    "INSERT INTO posts (id, url, title, submitter, isHit, time, flair) VALUES (:id, :url, :title, :submitter, :isHit, :time, :flair)"
    [ ":id" := unPostID (postId post),
      ":url" := postUrl post,
      ":title" := postTitle post,
      ":submitter" := postAuthor post,
      ":isHit" := (if hit then 1 else 0 :: Integer),
      ":time" := formatTime defaultTimeLocale "%F %T" (postCreatedTime post),
      ":flair" := fromMaybe "" (postFlairText post)
    ]
  close sql

-- | Generate HTML from the SQLite database
makeHtml :: IO Html
makeHtml = do
  sql <- getDataFileName (dbFileName config) >>= open
  hits <- query_ sql "SELECT id, url, title, submitter, time, flair FROM posts WHERE isHit = 1 ORDER BY time DESC LIMIT 50" :: IO [(Text, Text, Text, Text, Text, Text)]
  nonhits <- query_ sql "SELECT id, url, title, submitter, time, flair FROM posts WHERE isHit = 0 ORDER BY time DESC LIMIT 50" :: IO [(Text, Text, Text, Text, Text, Text)]
  -- Total number of rows
  n <- fromOnly . Prelude.head <$> (query_ sql "SELECT COUNT(*) FROM posts;" :: IO [Only Int])
  -- Total number of hits
  m <- fromOnly . Prelude.head <$> (query_ sql "SELECT COUNT(*) FROM posts WHERE isHit = 1;" :: IO [Only Int])
  close sql
  let mkTableRow :: (Text, Text, Text, Text, Text, Text) -> Html
      mkTableRow (pid, purl, ptitle, psubmitter, ptime, pflair) =
        H.tr $
          mapM_
            H.td
            [ H.code (toHtml pid),
              toHtml ptime,
              toHtml ("/u/" <> psubmitter),
              toHtml pflair,
              a ! A.href (textValue purl) $ toHtml ptitle
            ]
      tableHeader :: Html
      tableHeader =
        H.tr $ mapM_ (H.th . toHtml) ["Post ID" :: Text, "Time (UTC)", "Submitter", "Flair", "Title"]

  pure $ docTypeHtml $ do
    H.head $ do
      H.title "ApriBot"
      H.link ! A.rel "stylesheet" ! A.href "static/styles.css"
    H.body $ H.main $ do
      H.h1 "ApriBot"
      H.div ! A.class_ "prose" $ do
        H.p $ do
          "ApriBot monitors new posts on the "
          H.a ! A.href "https://reddit.com/r/pokemontrades" $ "/r/pokemontrades"
          " subreddit and identifies potential Aprimon-related threads. "
          "It does so by scanning for certain keywords in either the post title or body. "
          "This is admittedly not very sophisticated, and leads to quite a few false positives. "
          "In the long term, it would be nice to have a better algorithm for this."
        H.p $ do
          toHtml (printf "So far, ApriBot has seen a total of %d posts, " n :: String)
          toHtml (printf "of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          "This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        H.p $ do
          "If you have any questions about ApriBot, feel free to get in touch with "
          H.a ! A.href "https://reddit.com/u/is_a_togekiss" $ "/u/is_a_togekiss"
          "; she doesn’t have DMs enabled, so just comment on any of her posts."
        H.p $ do
          "ApriBot is implemented with Haskell and SQLite. If you’re interested, its source code is on "
          H.a ! A.href "https://github.com/penelopeysm/apribot" $ "GitHub"
          "."
      H.h2 "Hits"
      if null hits
        then H.p "None so far!"
        else H.table $ do
          tableHeader
          mapM_ mkTableRow (take 50 hits)
      H.h2 "Non-hits"
      if null nonhits
        then H.p "None so far!"
        else H.table $ do
          tableHeader
          mapM_ mkTableRow (take 50 nonhits)

-- | Thread for the web server
web :: MVar () -> IO ()
web lock = do
  css <- getDataFileName "static/styles.css"
  atomically lock $ printf "Launching web server on port %d...\n" (port config)
  Scotty.scotty (port config) $ do
    Scotty.get "/" $ do
      liftIO makeHtml >>= Scotty.html . renderHtml
    Scotty.get "/static/styles.css" $ do
      Scotty.setHeader "Content-Type" "text/css"
      Scotty.file css

-- | Thread to stream Reddit posts and process them
bot :: MVar () -> IO ()
bot lock = do
  atomically lock $ T.putStrLn "Starting Reddit bot..."
  credsUsername <- getEnvAsText "REDDIT_USERNAME"
  credsPassword <- getEnvAsText "REDDIT_PASSWORD"
  credsClientId <- getEnvAsText "REDDIT_ID"
  credsClientSecret <- getEnvAsText "REDDIT_SECRET"
  let creds = Credentials {..}
  env <- authenticate creds (userAgent config)
  let protected = do
        catch
          (runRedditT' env $ postStream defaultStreamSettings process lock (watchedSubreddit config))
          ( \(e :: SomeException) -> do
              atomically lock $ T.hPutStrLn stderr ("Exception: " <> T.pack (show e))
              threadDelay 5000000
              protected
          )
  protected

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  lock <- newMVar ()
  _ <- forkIO $ web lock
  bot lock
