module Web (web) where

import Config
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple
import Lucid
import Paths_apribot (getDataFileName)
import PostDatabase
import Text.Printf (printf)
import Utils
import qualified Web.Scotty as Scotty

tableHeaderSql :: Html ()
tableHeaderSql =
  tr_ $ mapM_ (th_ . toHtml) ["Post ID" :: Text, "Time (UTC)", "Submitter", "Flair", "Title"]

makeTableRowFromSql :: (Text, Text, Text, Text, Text, Text) -> Html ()
makeTableRowFromSql (pid, purl, ptitle, psubmitter, ptime, pflair) =
  tr_ $
    mapM_
      td_
      [ code_ (toHtml pid),
        toHtml ptime,
        toHtml ("/u/" <> psubmitter),
        toHtml pflair,
        a_ [href_ purl] $ toHtml ptitle
      ]

-- | HTML for the main page
mainHtml :: IO (Html ())
mainHtml = do
  sql <- getDataFileName (dbFileName config) >>= open
  hits <- getLatestHits 50 sql
  nonhits <- getLatestNonHits 50 sql
  n <- getTotalRows sql
  m <- getTotalHits sql
  close sql

  pure $ html_ $ do
    head_ $ do
      title_ "ApriBot"
      link_ [rel_ "stylesheet", href_ "static/styles.css"]
    body_ $ main_ $ do
      h1_ "ApriBot"
      div_ [class_ "prose"] $ do
        p_ $ do
          "ApriBot monitors new posts on the "
          a_ [href_ "https://reddit.com/r/pokemontrades"] "/r/pokemontrades"
          " subreddit and identifies potential Aprimon-related threads."
          " It does so by scanning for certain keywords in either the post title or body."
          " This is admittedly not very sophisticated, and leads to quite a few false positives."
          " In the long term, it would be nice to have a better algorithm for this."
        p_ $ do
          toHtml (printf "So far, ApriBot has processed a total of %d posts," n :: String)
          toHtml (printf " of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          " This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        p_ $ do
          "ApriBot is implemented with Haskell and SQLite."
          " If you’re interested, its source code is on "
          a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
          "."
          "If you have any questions about ApriBot, feel free to get in touch with me, either via GitHub or "
          a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
          "."
      h2_ "Hits"
      if null hits
        then p_ "None so far!"
        else table_ $ do
          tableHeaderSql
          mapM_ makeTableRowFromSql (take 50 hits)
      h2_ "Non-hits"
      if null nonhits
        then p_ "None so far!"
        else table_ $ do
          tableHeaderSql
          mapM_ makeTableRowFromSql (take 50 nonhits)

-- | Thread for the web server
web :: MVar () -> IO ()
web lock = do
  css <- getDataFileName "static/styles.css"
  atomically lock $ printf "Launching web server on port %d...\n" (port config)
  Scotty.scotty (port config) $ do
    Scotty.get "/" $ do
      liftIO mainHtml >>= Scotty.html . renderText
    Scotty.get "/static/styles.css" $ do
      Scotty.setHeader "Content-Type" "text/css"
      Scotty.file css
