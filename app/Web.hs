module Web (web) where

import Config
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple
import Paths_apribot (getDataFileName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Utils
import qualified Web.Scotty as Scotty

tableHeaderSql :: Html
tableHeaderSql =
  H.tr $ mapM_ (H.th . toHtml) ["Post ID" :: Text, "Time (UTC)", "Submitter", "Flair", "Title"]

makeTableRowFromSql :: (Text, Text, Text, Text, Text, Text) -> Html
makeTableRowFromSql (pid, purl, ptitle, psubmitter, ptime, pflair) =
  H.tr $
    mapM_
      H.td
      [ H.code (toHtml pid),
        toHtml ptime,
        toHtml ("/u/" <> psubmitter),
        toHtml pflair,
        a ! A.href (textValue purl) $ toHtml ptitle
      ]

-- | Generate HTML from the SQLite database
makeHtml :: IO Html
makeHtml = do
  sql <- getDataFileName (dbFileName config) >>= open
  hits <- query_ sql "SELECT id, url, title, submitter, time, flair FROM posts WHERE hit = 1 ORDER BY time DESC LIMIT 50" :: IO [(Text, Text, Text, Text, Text, Text)]
  nonhits <- query_ sql "SELECT id, url, title, submitter, time, flair FROM posts WHERE hit = 0 ORDER BY time DESC LIMIT 50" :: IO [(Text, Text, Text, Text, Text, Text)]
  -- Total number of rows
  n <- fromOnly . Prelude.head <$> (query_ sql "SELECT COUNT(*) FROM posts WHERE hit = 1 or hit = 0;" :: IO [Only Int])
  -- Total number of hits
  m <- fromOnly . Prelude.head <$> (query_ sql "SELECT COUNT(*) FROM posts WHERE hit = 1;" :: IO [Only Int])
  close sql

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
          " subreddit and identifies potential Aprimon-related threads."
          " It does so by scanning for certain keywords in either the post title or body."
          " This is admittedly not very sophisticated, and leads to quite a few false positives."
          " In the long term, it would be nice to have a better algorithm for this."
        H.p $ do
          toHtml (printf "So far, ApriBot has processed a total of %d posts," n :: String)
          toHtml (printf " of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          " This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        H.p $ do
          "ApriBot is implemented with Haskell and SQLite."
          " If you’re interested, its source code is on "
          H.a ! A.href "https://github.com/penelopeysm/apribot" $ "GitHub"
          "."
          "If you have any questions about ApriBot, feel free to get in touch with me, either via GitHub or "
          H.a ! A.href " https://reddit.com/u/is_a_togekiss" $ "Reddit"
          "."
      H.h2 "Hits"
      if null hits
        then H.p "None so far!"
        else H.table $ do
          tableHeaderSql
          mapM_ makeTableRowFromSql (take 50 hits)
      H.h2 "Non-hits"
      if null nonhits
        then H.p "None so far!"
        else H.table $ do
          tableHeaderSql
          mapM_ makeTableRowFromSql (take 50 nonhits)

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
