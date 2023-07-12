module Web (web) where

import CMarkGFM
import Config
import Control.Concurrent (MVar)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Password.Bcrypt
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (secondsToDiffTime)
import Database
import Database.SQLite.Simple
import Lucid
import Paths_apribot (getDataFileName)
import Reddit
import Reddit.Auth (Token (..))
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Printf (printf)
import Utils
import qualified Web.Cookie as C
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

hashedStateCookieName :: Text
hashedStateCookieName = "apribotOauth2State"

tokenCookieName :: Text
tokenCookieName = "apribotToken"

-- * Storage of OAuth2 tokens in memory

-- * Generating RedditEnv values to use

-- | Retrieve token-identifier cookie, and thus the token from the database, if
-- the user has one.
retrieveRedditEnv :: S.ActionM (Maybe RedditEnv)
retrieveRedditEnv = do
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie of
    Nothing -> pure Nothing
    Just sessionId -> do
      maybeToken <- liftIO $ getToken sessionId
      case maybeToken of
        Just t -> Just <$> liftIO (mkEnvFromToken t (userAgent config))
        Nothing -> pure Nothing

-- | Make a new RedditEnv by requesting a token. This is to be used on the
-- redirect URI and assumes that the user has been redirected to here after
-- granting access on Reddit (i.e. the URI query params contains an
-- authorisation code which can be used to request an access token). If the
-- params are not present then this redirects the user back to "/".
makeNewRedditEnv :: Text -> Text -> Text -> S.ActionM ()
makeNewRedditEnv clientId clientSecret redirectUri = do
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
            C.setCookieSameSite = Just C.sameSiteLax,
            C.setCookieSecure = True
          }
      -- Store the token in the database using this session ID
      liftIO $ addToken sessionId t
    _ -> do
      S.redirect "auth_error"

-- | Deletes all cookies and the corresponding token from the database (if it
-- exists)
cleanup :: S.ActionM ()
cleanup = do
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie of
    Just cookie -> do
      liftIO $ removeToken cookie
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
        a_ [href_ purl] $ toHtmlRaw $ sanitizeBalance ptitle
      ]

-- | Reusable <head> element lead element for HTML page
headHtml :: Maybe Text -> Bool -> Html ()
headHtml titleExtra withJS = do
  head_ $ do
    title_ $ toHtml (maybe "ApriBot" ("ApriBot :: " <>) titleExtra)
    link_ [rel_ "stylesheet", href_ "static/styles.css"]
    link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "static/apple-touch-icon.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "static/favicon-32x32.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "static/favicon-16x16.png"]
    link_ [rel_ "manifest", href_ "static/site.webmanifest"]
    if withJS
      then script_ [src_ "static/enableButton.js", type_ "module"] ("" :: Text)
      else mempty

-- | Error HTML
errorHtml :: SomeException -> Html ()
errorHtml e = do
  headHtml (Just "Error") False
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
  sqlFileName <- getSqlFileName
  (hits, nonhits, n, m) <- liftIO $ withConnection sqlFileName $ \sql -> do
    hits <- getLatestHits 50 sql
    nonhits <- getLatestNonHits 50 sql
    n <- getTotalMLAssignedRows sql
    m <- getTotalMLAssignedHits sql
    pure (hits, nonhits, n, m)

  pure $ doctypehtml_ $ do
    headHtml Nothing False
    body_ $ main_ $ do
      h1_ "ApriBot"
      div_ [class_ "prose"] $ do
        p_ $ do
          "ApriBot monitors new posts on the "
          a_ [href_ "https://reddit.com/r/pokemontrades"] "/r/pokemontrades"
          " subreddit and identifies potential Aprimon-related threads, using a machine learning algorithm trained on 7709 "
          a_ [href_ "/contribute"] "manually labelled"
          " posts from /r/pokemontrades."
        p_ $ do
          toHtml (printf "So far, ApriBot's new algorithm has processed a total of %d posts," n :: String)
          toHtml (printf " of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          " This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        p_ $ do
          "ApriBot is implemented with Haskell and SQLite; the ML bits are written in Python."
          " If you’re interested, its source code is on "
          a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
          ". (Issues and pull requests are welcome!) "
          "If you have any questions about ApriBot, feel free to get in touch with me, either via "
          a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
          " or "
          a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
          "."
        p_ $ do
          b_ "While the current algorithm has quite good performance (>93% cross-validation F1 score), I might be able to make it even better in the future with more data."
          " If you are interested and have a few minutes to spare, do head over to the "
          a_ [href_ "/contribute"] "contribute page"
          "—I will be very grateful!"
      h2_ "Hits"
      if null hits
        then p_ "None so far!"
        else table_ $ do
          thead_ tableHeaderSql
          tbody_ $ mapM_ makeTableRowFromSql (take 50 hits)
      h2_ "Non-hits"
      if null nonhits
        then p_ "None so far!"
        else table_ $ do
          thead_ tableHeaderSql
          tbody_ $ mapM_ makeTableRowFromSql (take 50 nonhits)

logoutHtml :: Html ()
logoutHtml = do
  headHtml (Just "Logged out") False
  body_ $ main_ $ do
    h1_ "Logged out"
    p_ "You have been logged out. Thank you so much for your time!"
    p_ $ do
      "Return to the "
      a_ [href_ "/"] "home page"
      ", or the "
      a_ [href_ "/contribute"] "contribute page"
      "."

privacyHtml :: Html ()
privacyHtml = do
  headHtml (Just "Privacy") False
  body_ $ main_ $ do
    h1_ "Privacy"
    p_ $ i_ $ do
      "("
      a_ [href_ "/contribute"] "back to contribute page"
      ")"
    h2_ "Personal information"
    p_ $ do
      "The only personal information ApriBot stores about you is your Reddit username (on posts which you have labelled). "
      "This information is required to make sure that you are not asked to label the same post twice. "
      "It is not shared with anybody else, and your votes can only be viewed by you. "
    h2_ "Cookies"
    p_ $ do
      "ApriBot uses one cookie to log you into Reddit securely (using OAuth), and another to store your login status so that you don't have to keep logging in, but otherwise does not perform any other tracking. "
      "If you want to verify this, the source code of ApriBot can be viewed on "
      a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
      "."
    h2_ "Contact me..."
    p_ $ do
      "...via "
      a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
      " or "
      a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
      "."

authErrorHtml :: Html ()
authErrorHtml = do
  headHtml (Just "Error") False
  body_ $ main_ $ do
    h1_ "Authentication error :("
    p_ $ do
      "Sorry! There was an error logging you in. "
      "This might be because you denied ApriBot access, or you took too long to log in (you have to do so within 10 minutes of opening the page)."
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contribErrorHtml :: Html ()
contribErrorHtml = do
  headHtml (Just "Error") False
  body_ $ main_ $ do
    h1_ "Form submission error :("
    p_ $ do
      "Sorry! There was an error recording your vote. "
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contributingLoggedOutHtml :: Int -> Int -> Html ()
contributingLoggedOutHtml totalPosts labelledPosts = do
  headHtml (Just "Contributing") False
  body_ $ main_ $ do
    h1_ "Contribute"
    p_ $ i_ $ do
      "("
      a_ [href_ "/"] "back to home page"
      " — "
      a_ [href_ "/privacy"] "privacy"
      ")"
    p_ $ do
      "Machine learning algorithms, such as the one ApriBot uses, need to be trained on "
      i_ "labelled data"
      ": that is, posts which have been manually classified (by experts—yes, that's you!) as being either Aprimon-related or not. "
      "Right now, out of a total of "
      toHtml (show totalPosts)
      " posts, "
      toHtml (show labelledPosts)
      " have been labelled; 7709 of these were used to train ApriBot's current algorithm. "
      "If you have a few minutes to spare, please consider helping me out by increasing this number!"
    div_ [id_ "login-link-container"] $ do
      a_ [href_ "/login", id_ "login-link"] "Log in with Reddit"
    p_ $ do
      b_ "ApriBot uses cookies only to log you in and to keep you logged in. "
      b_ "By logging in, it is assumed that you consent to this. "
      "See the "
      a_ [href_ "/privacy"] "privacy page"
      " for more information."

contributingLoggedInHtml ::
  Text ->
  Int ->
  Int ->
  Int ->
  Maybe (Text, Text, Text, Text, Text, Text, Text) ->
  Html ()
contributingLoggedInHtml username totalPosts labelledPosts labelledPostsByUser nextPost = do
  headHtml (Just "Contributing") True
  body_ $ main_ $ do
    h1_ "Contribute"
    p_ $ i_ $ do
      "("
      a_ [href_ "/"] "back to home page"
      " — "
      a_ [href_ "/privacy"] "privacy"
      " — "
      a_ [href_ "/your_votes"] "view your votes"
      " — "
      a_ [href_ "/logout"] "logout"
      ")"
    p_ $ b_ $ do
      "You are now logged in as: /u/"
      toHtml username
    p_ $ do
      "Right now, out of a total of "
      toHtml (show totalPosts)
      " posts, "
      toHtml (show labelledPosts)
      " have been labelled. "
      if labelledPostsByUser > 0
        then do
          toHtml $ show labelledPostsByUser
          " of these were by you. Thank you so much! <3"
        else ""
      case nextPost of
        -- This is very optimistic...
        Nothing -> do
          hr_ []
          p_ "There are no more unlabelled posts. Please check back again tomorrow!"
        Just (postId, postUrl, postTitle, postBody, postSubmitter, postTime, postFlair) -> do
          div_ [class_ "form-container"] $ do
            form_ [class_ "aprimon-question", action_ "/contribute", method_ "post"] $ do
              span_ $ b_ "Is the post below offering, or looking for, non-shiny breedable Aprimon?"
              input_ [type_ "hidden", name_ "id", value_ postId]
              input_ [type_ "hidden", name_ "username", value_ username]
              div_ [id_ "button-container"] $ do
                button_ [type_ "submit", name_ "vote", value_ "1", disabled_ ""] "✅ Yes"
                button_ [type_ "submit", name_ "vote", value_ "0", disabled_ ""] "❌ No"
                button_ [type_ "submit", name_ "vote", value_ "2", disabled_ ""] "⏭️ Skip"
          div_ $ do
            span_ [class_ "title"] $ toHtmlRaw $ sanitizeBalance postTitle
            span_ [class_ "boxed-flair"] $ toHtml postFlair
          ul_ $ do
            li_ $ toHtml (printf "Submitted by /u/%s at %s UTC" postSubmitter postTime :: String)
            li_ $ do
              a_ [href_ postUrl] "Link to original Reddit post"
          if T.null (T.strip postBody)
            then p_ "<empty post body>"
            else
              div_ [class_ "post-body"] $
                toHtmlRaw $
                  commonmarkToHtml [optSmart] [extTable, extStrikethrough] postBody

yourVotesHtml :: Int -> Text -> [(Text, Text, Text, Text, Int)] -> Html ()
yourVotesHtml totalLabelledByUser username votes = do
  let makeTableRow :: (Text, Text, Text, Text, Int) -> Html ()
      makeTableRow (postId, postTitle, postUrl, postSubmitter, vote) =
        tr_ $ do
          td_ $ code_ $ toHtml postId
          td_ $ a_ [href_ postUrl] (toHtmlRaw $ sanitizeBalance postTitle)
          td_ $ toHtml $ "/u/" <> postSubmitter
          td_ $ case vote of
            1 -> "Yes"
            0 -> "No"
            _ -> error "Vote that wasn't 0 or 1 found: this should not happen!"
  headHtml (Just "Your votes") False
  body_ $ main_ $ do
    h1_ "Your votes"
    p_ $ i_ $ do
      "("
      a_ [href_ "/contribute"] "back to contributing page"
      " — "
      a_ [href_ "/privacy"] "privacy"
      " — "
      a_ [href_ "/logout"] "logout"
      ")"
    p_ $ b_ $ do
      "You are now logged in as: /u/"
      toHtml username
    case votes of
      [] -> p_ "You have not labelled any posts yet."
      _ -> do
        p_ $ do
          "In total, you have labelled "
          toHtml (show totalLabelledByUser)
          " posts. Here are "
          if totalLabelledByUser > length votes
             then do
                "the last "
                toHtml (show $ length votes)
             else "all the"
          " posts you have labelled (most recent on top). Thank you so much for your help!"
        p_ $ do
          "If you find you need to change or delete any of your votes, please get in touch with me via "
          a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
          "."
        table_ $ do
          thead_ $ do
            tr_ $ do
              th_ "Post ID"
              th_ "Post title"
              th_ "Submitter"
              th_ "Your vote"
          tbody_ $ do
            mapM_ makeTableRow votes

-- | Thread for the web server
web :: MVar () -> IO ()
web lock = do
  clientId <- getEnvAsText "REDDIT_FE_ID"
  clientSecret <- getEnvAsText "REDDIT_FE_SECRET"
  staticDir <- getDataFileName "static"
  atomically lock $ printf "Launching web server on port %d...\n" (port config)

  S.scotty (port config) $ do
    S.get "/" $ do
      liftIO mainHtml >>= S.html . renderText

    S.get "/static/styles.css" $ do
      S.setHeader "Content-Type" "text/css"
      S.file (staticDir <> "/" <> "styles.css")

    S.get "/static/site.webmanifest" $ do
      S.setHeader "Content-Type" "application/manifest+json"
      S.file (staticDir <> "/" <> "site.webmanifest")

    S.get "/static/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file (staticDir <> "/" <> "favicon.ico")

    S.get "/static/favicon-16x16.png" $ do
      S.setHeader "Content-Type" "image/x-png"
      S.file (staticDir <> "/" <> "favicon-16x16.png")

    S.get "/static/favicon-32x32.png" $ do
      S.setHeader "Content-Type" "image/x-png"
      S.file (staticDir <> "/" <> "favicon-32x32.png")

    S.get "/static/apple-touch-icon.png" $ do
      S.setHeader "Content-Type" "image/x-png"
      S.file (staticDir <> "/" <> "apple-touch-icon.png")

    S.get "/static/android-chrome-192x192.png" $ do
      S.setHeader "Content-Type" "image/x-png"
      S.file (staticDir <> "/" <> "android-chrome-192x192.png")

    S.get "/static/android-chrome-512x512.png" $ do
      S.setHeader "Content-Type" "image/x-png"
      S.file (staticDir <> "/" <> "android-chrome-512x512.png")

    S.get "/static/enableButton.js" $ do
      S.setHeader "Content-Type" "text/javascript"
      S.file (staticDir <> "/" <> "enableButton.js")

    S.get "/authorised" $ do
      makeNewRedditEnv clientId clientSecret (redirectUri config)
      S.redirect "/contribute"

    S.get "/auth_error" $ do
      S.html $ renderText $ doctypehtml_ authErrorHtml

    S.get "/contrib_error" $ do
      S.html $ renderText $ doctypehtml_ contribErrorHtml

    S.get "/login" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        Just _ -> S.redirect "/contribute"
        Nothing -> do
          -- Generate a random state for the user, and store it as a cookie
          state <- liftIO $ randomText 40
          hash <- liftIO $ unPasswordHash <$> hashPassword (mkPassword state)
          let stateCookie = SC.makeSimpleCookie hashedStateCookieName hash
          SC.setCookie $
            stateCookie
              { C.setCookieHttpOnly = True,
                C.setCookieSecure = True,
                C.setCookieSameSite = Just C.sameSiteLax,
                C.setCookieMaxAge = Just (secondsToDiffTime 600)
              }
          -- Redirect to Reddit's auth page
          S.redirect $
            fromStrict $
              mkRedditAuthURL
                False
                AuthUrlParams
                  { authUrlClientID = clientId,
                    authUrlState = state,
                    authUrlRedirectUri = redirectUri config,
                    authUrlDuration = Temporary,
                    authUrlScopes = Set.singleton ScopeIdentity
                  }

    S.get "/logout" $ do
      -- Remove token from database
      maybeCookie <- SC.getCookie tokenCookieName
      case maybeCookie of
        Nothing -> S.redirect "/"
        Just _ -> do
          cleanup
          S.html $ renderText $ doctypehtml_ logoutHtml

    S.post "/contribute" $ do
      mPostId :: Maybe Text <- (Just <$> S.param "id") `S.rescue` const (pure Nothing)
      mVoter :: Maybe Text <- (Just <$> S.param "username") `S.rescue` const (pure Nothing)
      mVote :: Maybe Int <- (Just <$> S.param "vote") `S.rescue` const (pure Nothing)
      case (mPostId, mVoter, mVote) of
        (Just postId, Just voter, Just vote) ->
          if vote /= 0 && vote /= 1
            then S.redirect "/contribute"
            else do
              sqlFileName <- getSqlFileName
              liftIO $ withConnection sqlFileName $ addVote postId voter vote
              S.redirect "/contribute"
        _ -> S.redirect "/contrib_error"

    S.get "/contribute" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        -- User is not logged in
        Nothing -> do
          sqlFileName <- getSqlFileName
          (totalPosts, labelledPosts) <- liftIO $ withConnection sqlFileName $ \sql -> do
            (,)
              <$> getTotalRows sql
              <*> getTotalNumberLabelled sql
          S.html $ renderText $ doctypehtml_ $ do
            contributingLoggedOutHtml totalPosts labelledPosts
        -- User is logged in
        Just env -> do
          usernameEither <- liftIO $ try $ runRedditT' env (accountUsername <$> myAccount)
          case usernameEither of
            Left e -> do
              cleanup
              S.html $ renderText $ errorHtml e
            Right username -> do
              -- Get data from database
              sqlFileName <- getSqlFileName
              (totalPosts, labelledPosts, labelledPostsByUser, nextPost) <- liftIO $ withConnection sqlFileName $ \sql ->
                do
                  (,,,)
                  <$> getTotalRows sql
                  <*> getTotalNumberLabelled sql
                  <*> getNumberLabelledBy username sql
                  <*> getNextUnlabelledPost sql
              -- Serve HTML
              S.html $ renderText $ doctypehtml_ $ do
                contributingLoggedInHtml username totalPosts labelledPosts labelledPostsByUser nextPost

    S.get "/privacy" $ do
      S.html $ renderText $ doctypehtml_ privacyHtml

    S.get "/your_votes" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        -- User is not logged in
        Nothing -> do
          S.redirect "/contribute"

        -- User is logged in
        Just env -> do
          usernameEither <- liftIO $ try $ runRedditT' env (accountUsername <$> myAccount)
          case usernameEither of
            Left e -> do
              cleanup
              S.html $ renderText $ errorHtml e
            Right username -> do
              -- Get data from database
              sqlFileName <- getSqlFileName
              (votes, nLabelledByUser) <- liftIO $ withConnection sqlFileName $ \sql ->
                (,)
                  <$> getLastNVotesBy 100 username sql
                  <*> getNumberLabelledBy username sql
              -- Serve HTML
              S.html $ renderText $ doctypehtml_ $ do
                yourVotesHtml nLabelledByUser username votes
