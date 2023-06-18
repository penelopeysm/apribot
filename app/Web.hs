module Web (web) where

import CMarkGFM
import Config
import Control.Applicative (liftA2)
import Control.Concurrent (MVar)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as M
import Data.Password.Bcrypt
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import Database
import Database.SQLite.Simple
import Lucid
import Paths_apribot (getDataFileName)
import Reddit
import Reddit.Auth (Token (..))
import Text.Printf (printf)
import qualified Data.Text as T
import Utils
import qualified Web.Cookie as C
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

hashedStateCookieName :: Text
hashedStateCookieName = "apribotOauth2State"

tokenCookieName :: Text
tokenCookieName = "apribotToken"

-- * Storage of OAuth2 tokens in memory

-- TODO: Deal with tokens which have expired
type Database = M.Map Text Token

addToTokenDb :: Text -> Token -> IORef Database -> IO ()
addToTokenDb userSessionId authToken dbRef = do
  atomicModifyIORef' dbRef (\db -> (M.insert userSessionId authToken db, ()))

removeFromTokenDb :: Text -> IORef Database -> IO ()
removeFromTokenDb userSessionId dbRef = do
  atomicModifyIORef' dbRef (\db -> (M.delete userSessionId db, ()))

-- * Generating RedditEnv values to use

-- | Retrieve token-identifier cookie, and thus the token from the database, if
-- the user has one.
retrieveRedditEnv :: IORef Database -> S.ActionM (Maybe RedditEnv)
retrieveRedditEnv dbRef = do
  db <- liftIO $ readIORef dbRef
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie >>= (`M.lookup` db) of
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

-- | Deletes all cookies and the corresponding token from the database (if it
-- exists)
cleanup :: IORef Database -> S.ActionM ()
cleanup dbRef = do
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie of
    Just cookie -> do
      liftIO $ removeFromTokenDb cookie dbRef
      SC.deleteCookie tokenCookieName
      SC.deleteCookie hashedStateCookieName
    Nothing -> pure ()

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
    title_ $ toHtml (maybe "ApriBot" ("ApriBot :: " <>) titleExtra)
    link_ [rel_ "stylesheet", href_ "static/styles.css"]

-- | Error HTML
errorHtml :: SomeException -> Html ()
errorHtml e = do
  headHtml (Just "Error")
  body_ $ do
    h1_ "An error occurred :("
    p_ $ do
      "Please report this to me, either via "
      a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
      " or "
      a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
      "."
    p_ $ code_ $ toHtml $ show e

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
        p_ $ do
          toHtml (printf "So far, ApriBot has processed a total of %d posts," n :: String)
          toHtml (printf " of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          " This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        p_ $ do
          "ApriBot is implemented with Haskell and SQLite."
          " If you’re interested, its source code is on "
          a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
          ". (Issues and pull requests are welcome!) "
          "If you have any questions about ApriBot, feel free to get in touch with me, either via GitHub or "
          a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
          "."
        p_ $ do
          b_ "I am currently looking for people to help me improve ApriBot's hit-detection algorithm."
          " If you are interested and have a few minutes to spare, head over to the "
          a_ [href_ "/contribute"] "contribute page"
          ". I will be very grateful!"
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

logoutHtml :: Html ()
logoutHtml = do
  headHtml (Just "Logged out")
  body_ $ main_ $ do
    h1_ "Logged out"
    p_ "You have been logged out. Thank you so much for your time!"
    p_ $ do
      "Return to the "
      a_ [href_ "/"] "home page"
      ", or the "
      a_ [href_ "/contribute"] "contribute page"
      "."

authErrorHtml :: Html ()
authErrorHtml = do
  headHtml (Just "Error")
  body_ $ main_ $ do
    h1_ "Authentication error :("
    p_ $ do
      "Sorry! There was an error logging you in. "
      "This might be because you denied ApriBot access, or you took too long to log in (you have to do so within 10 minutes of opening the page)."
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contribErrorHtml :: Html ()
contribErrorHtml = do
  headHtml (Just "Error")
  body_ $ main_ $ do
    h1_ "Form submission error :("
    p_ $ do
      "Sorry! There was an error recording your vote. "
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contributingLoggedOutHtml :: Text -> Html ()
contributingLoggedOutHtml redditUrl = do
  headHtml (Just "Contributing")
  body_ $ main_ $ do
    h1_ "Contribute"
    p_ $ i_ $ a_ [href_ "/"] "(back to home page)"
    p_ $ do
      "Right now, ApriBot uses a very primitive keyword-searching system for identifying Aprimon-related posts. "
      "My goal is to eventually replace this with some sort of machine learning algorithm. "
    p_ $ do
      "However, to do this, I need "
      i_ "labelled data"
      ": that is, a number of posts which have been manually classified (by experts—yes, that's you!) as being either Aprimon-related or not. "
      "If you have a few minutes to spare, please consider helping me out by labelling some posts."
    p_ $ do
      "To do this, you will need to "
      b_ $ a_ [href_ redditUrl] "log in with Reddit"
      "."
    p_ $ do
      "The permissions I am requesting do not give me access any of your personal information, apart from your Reddit username and the time you created your account. "
      "I only need this to make sure that you don't label the same post multiple times."

contributingLoggedInHtml ::
  Text ->
  Int ->
  Maybe (Text, Text, Text, Text, Text, Text, Text) ->
  Html ()
contributingLoggedInHtml username nLabelled nextPost = do
  headHtml (Just "Contributing")
  body_ $ main_ $ do
    h1_ "Contribute"
    p_ $ i_ $ do
      "("
      a_ [href_ "/"] "back to home page"
      " — "
      a_ [href_ "/logout"] "logout"
      ")"
    p_ $ do
      span_ $ b_ $ do
        "You are now logged in as: /u/"
        toHtml username
        "."
      " You have labelled a total of "
      toHtml $ show nLabelled
      " post"
      toHtml $ if nLabelled == 1 then "." else "s." :: Text
      toHtml $ if nLabelled > 0 then " Thank you so much! <3" else "" :: Text
      case nextPost of
        -- This is very optimistic...
        Nothing -> do
          hr_ []
          p_ "There are no more unlabelled posts. Please check back again tomorrow!"
        Just (postId, postUrl, postTitle, postBody, postSubmitter, postTime, postFlair) -> do
          div_ [class_ "form-container"] $ do
            form_ [class_ "aprimon-question", action_ "/contribute", method_ "post"] $ do
              span_ $ b_ "Is the post below related?"
              input_ [type_ "hidden", name_ "id", value_ postId]
              input_ [type_ "hidden", name_ "username", value_ username]
              button_ [type_ "submit", name_ "vote", value_ "1"] "Yes"
              button_ [type_ "submit", name_ "vote", value_ "0"] "No"
          div_ $ do
            span_ [class_ "title"] $ toHtml postTitle
            span_ [class_ "boxed-flair"] $ toHtml postFlair
          ul_ $ do
            li_ $ toHtml (printf "Submitted by /u/%s at %s UTC" postSubmitter postTime :: String)
            li_ $ do
              a_ [href_ postUrl] "Link to original Reddit post"
          if T.null (T.strip postBody)
            then p_ "<empty post body>"
            else div_ [class_ "post-body"] $
              toHtmlRaw $
                commonmarkToHtml [optSmart] [extTable, extStrikethrough] postBody

-- | Thread for the web server
web :: MVar () -> IO ()
web lock = do
  clientId <- getEnvAsText "REDDIT_FE_ID"
  clientSecret <- getEnvAsText "REDDIT_FE_SECRET"
  css <- getDataFileName "static/styles.css"
  dbRef <- newIORef M.empty
  atomically lock $ printf "Launching web server on port %d...\n" (port config)

  S.scotty (port config) $ do
    S.get "/" $ do
      liftIO mainHtml >>= S.html . renderText

    S.get "/static/styles.css" $ do
      S.setHeader "Content-Type" "text/css"
      S.file css

    S.get "/authorised" $ do
      makeNewRedditEnv clientId clientSecret (redirectUri config) dbRef
      S.redirect "/contribute"

    S.get "/auth_error" $ do
      S.html $ renderText $ html_ authErrorHtml

    S.get "/contrib_error" $ do
      S.html $ renderText $ html_ contribErrorHtml

    S.get "/logout" $ do
      -- Remove token from database
      maybeCookie <- SC.getCookie tokenCookieName
      case maybeCookie of
        Nothing -> S.redirect "/"
        Just _ -> do
          cleanup dbRef
          S.html $ renderText $ html_ logoutHtml

    S.post "/contribute" $ do
      mPostId :: Maybe Text <- (Just <$> S.param "id") `S.rescue` const (pure Nothing)
      mVoter :: Maybe Text <- (Just <$> S.param "username") `S.rescue` const (pure Nothing)
      mVote :: Maybe Int <- (Just <$> S.param "vote") `S.rescue` const (pure Nothing)
      case (mPostId, mVoter, mVote) of
        (Just postId, Just voter, Just vote) -> do
          liftIO $ atomically lock $ do
            putStrLn $ printf "Received vote from /u/%s on post %s: %d" voter postId vote
            sql <- getDataFileName (dbFileName config) >>= open
            addVote postId voter vote sql
            n <- getNumVotes sql
            close sql
            putStrLn $ printf "Total number of votes: %d" n
          S.redirect "/contribute"
        _ -> S.redirect "/contrib_error"

    S.get "/contribute" $ do
      maybeEnv <- retrieveRedditEnv dbRef
      case maybeEnv of
        -- User is not logged in
        Nothing -> do
          -- Generate a random state for the user, and store it as a cookie
          state <- liftIO $ randomText 40
          hash <- liftIO $ unPasswordHash <$> hashPassword (mkPassword state)
          let stateCookie = SC.makeSimpleCookie hashedStateCookieName hash
          SC.setCookie $
            stateCookie
              { C.setCookieHttpOnly = True,
                -- Secure works with localhost over HTTP in Firefox and Chrome
                C.setCookieSecure = True,
                -- Surely 10 minutes is enough for the user to log in
                C.setCookieMaxAge = Just (secondsToDiffTime 600)
              }
          S.html $ renderText $ html_ $ do
            contributingLoggedOutHtml $
              mkRedditAuthURL $
                AuthUrlParams
                  { authUrlClientID = clientId,
                    authUrlState = Just state,
                    authUrlRedirectUri = redirectUri config,
                    authUrlDuration = Temporary,
                    authUrlScopes = Set.singleton ScopeIdentity
                  }

        -- User is logged in
        Just env -> do
          usernameEither <- liftIO $ try $ runRedditT' env (accountUsername <$> myAccount)
          case usernameEither of
            Left e -> do
              cleanup dbRef
              S.html $ renderText $ errorHtml e
            Right username -> do
              -- Get data from database
              sql <- liftIO $ getDataFileName (dbFileName config) >>= open
              nLabelled <- liftIO $ getNumberLabelled username sql
              nextPost <- liftIO $ getNextUnlabelledPost sql
              liftIO $ close sql
              -- Serve HTML
              S.html $ renderText $ html_ $ do
                contributingLoggedInHtml username nLabelled nextPost
