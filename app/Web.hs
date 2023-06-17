module Web (web) where

import Config
import Control.Applicative (liftA2)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.List as List
import Data.Password.Bcrypt
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Database.SQLite.Simple
import Lucid
import Paths_apribot (getDataFileName)
import Database
import Reddit
import Reddit.Auth (Token (..))
import Text.Printf (printf)
import Utils
import qualified Web.Cookie as C
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

hashedStateCookieName :: Text
hashedStateCookieName = "apribotOauth2State"

tokenCookieName :: Text
tokenCookieName = "apribotToken"

-- TODO: Deal with tokens which have expired
type Database = [(Text, Token)]

empty :: Database
empty = []

addToTokenDb :: Text -> Token -> IORef Database -> IO ()
addToTokenDb userSessionId authToken dbRef = do
  atomicModifyIORef' dbRef (\db -> ((userSessionId, authToken) : db, ()))

-- | Retrieve authentication code from cookie, and thus the token from the
-- database, if the user has one.
retrieveRedditEnv :: IORef Database -> S.ActionM (Maybe RedditEnv)
retrieveRedditEnv dbRef = do
  db <- liftIO $ readIORef dbRef
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie >>= (`List.lookup` db) of
    Just t -> Just <$> liftIO (newEnv t (userAgent config))
    Nothing -> pure Nothing

-- | Make a new RedditEnv by requesting a token. This is to be used on the
-- redirect URI and assumes that the user has been redirected to here after
-- granting access on Reddit (i.e. the URI query params contains an
-- authorisation code which can be used to request an access token). If the
-- params are not present then this redirects the user back to "/".
makeNewRedditEnv :: Text -> Text -> Text -> IORef Database -> S.ActionM ()
makeNewRedditEnv clientId clientSecret redirectUri dbRef = do
  -- Retrieve stored state from the cookie
  hashedState <- fmap PasswordHash <$> SC.getCookie hashedStateCookieName
  -- Get the authentication code and state from the URI query parameters, and
  -- check that they're correct
  authCodeMaybe <- (Just <$> S.param "code") `S.rescue` const (pure Nothing)
  returnedState <- (Just <$> S.param "state") `S.rescue` const (pure Nothing)
  case (authCodeMaybe, liftA2 checkPassword (mkPassword <$> returnedState) hashedState) of
    (Just authCode, Just PasswordCheckSuccess) -> do
      -- Get rid of the state
      SC.deleteCookie hashedStateCookieName
      -- Authenticate using the code that we just got
      let creds =
            CodeGrantCredentials
              { codeGrantClientId = clientId,
                codeGrantClientSecret = clientSecret,
                codeGrantRedirectUri = redirectUri,
                codeGrantCode = authCode
              }
      env <- liftIO $ authenticate creds (userAgent config)
      t <- liftIO $ runRedditT' env getTokenFromEnv
      -- Generate a random session ID for the user and store it in a cookie
      sessionId <- liftIO $ randomText 60
      let sessionCookie = SC.makeSimpleCookie tokenCookieName sessionId
      SC.setCookie $
        sessionCookie
          { C.setCookieExpires = Just (tokenExpiresAt t),
            C.setCookieHttpOnly = True,
            C.setCookieSecure = True
          }
      -- Store the token in the 'database' using this session ID
      liftIO $ addToTokenDb sessionId t dbRef
    _ -> do
      S.redirect "auth_error"

-- * HTML helpers

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

-- | Reusable <head> element lead element for HTML page
headHtml :: Maybe Text -> Html ()
headHtml titleExtra = do
  head_ $ do
    title_ $ toHtml (maybe "ApriBot" ("ApriBot | " <>) titleExtra)
    link_ [rel_ "stylesheet", href_ "static/styles.css"]

-- * HTML that is actually served

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
    headHtml Nothing
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

-- | HTML for login page
loginHtml :: Text -> Text -> Text -> Html ()
loginHtml clientId state redirectUri =
  let redditUrl =
        mkRedditAuthURL $
          AuthUrlParams
            { authUrlClientID = clientId,
              authUrlState = Just state,
              authUrlRedirectUri = redirectUri,
              authUrlDuration = Temporary,
              authUrlScopes = Set.singleton ScopeIdentity
            }
   in html_ $ do
        headHtml (Just "Login")
        body_ $ main_ $ do
          p_ "To access this page, you need to log in with Reddit."
          a_ [href_ redditUrl] $ h2_ $ p_ "Click here to do so."

-- | HTML to show the user's details
timeHtml :: Text -> UTCTime -> Html ()
timeHtml username createdTime = do
  headHtml (Just "Success")
  body_ $ do
    p_ $ toHtml $ "Welcome~ You are now logged in as user: " <> username <> "."
    p_ $ toHtml $ "You have been on Reddit since: " <> show createdTime <> "!"

-- * Main page

-- | Thread for the web server
web :: MVar () -> IO ()
web lock = do
  clientId <- getEnvAsText "REDDIT_FE_ID"
  clientSecret <- getEnvAsText "REDDIT_FE_SECRET"
  css <- getDataFileName "static/styles.css"
  dbRef <- newIORef empty
  atomically lock $ printf "Launching web server on port %d...\n" (port config)

  S.scotty (port config) $ do
    S.get "/" $ do
      liftIO mainHtml >>= S.html . renderText

    S.get "/static/styles.css" $ do
      S.setHeader "Content-Type" "text/css"
      S.file css

    S.get "/login" $ do
      state <- liftIO $ randomText 24
      hash <- liftIO $ unPasswordHash <$> hashPassword (mkPassword state)
      let stateCookie = SC.makeSimpleCookie hashedStateCookieName hash
      SC.setCookie $
        stateCookie
          { C.setCookieHttpOnly = True,
            -- Secure works with localhost over HTTP in Firefox and Chrome
            C.setCookieSecure = True,
            -- Surely 5 minutes is enough for the user to log in
            C.setCookieMaxAge = Just (secondsToDiffTime 300)
          }
      S.html $ renderText $ loginHtml clientId state (redirectUri config)

    S.get "/authorised" $ do
      makeNewRedditEnv clientId clientSecret (redirectUri config) dbRef
      S.redirect "/time"

    S.get "/auth_error" $ do
      S.html $ renderText $ html_ $ do
        headHtml (Just "Error")
        body_ $ main_ $ do
          p_ "There was an error logging you in."
          a_ [href_ "/login"] $ h2_ $ p_ "Please try again."

    S.get "/time" $ do
      maybeEnv <- retrieveRedditEnv dbRef
      case maybeEnv of
        Nothing -> S.redirect "/login"
        Just env -> do
          (username, createdTime) <- liftIO $ runRedditT' env $ do
            me <- myAccount
            pure (accountUsername me, accountCreatedTime me)
          -- Show it to the user
          S.html $ renderText (timeHtml username createdTime)
