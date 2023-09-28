module Web (web) where

import CMarkGFM
import Control.Applicative (liftA2)
import Control.Exception (SomeException, try)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Password.Bcrypt
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (getCurrentTime, secondsToDiffTime, UTCTime (..))
import Database
import DiscordBot (notifyDiscord)
import Lucid
import Reddit
import Reddit.Auth (Token (..), refresh)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Printf (printf)
import Trans
import Utils
import qualified Web.Cookie as C
import qualified Web.Scotty.Cookie as SC
import qualified Web.Scotty.Trans as ST

hashedStateCookieName :: Text
hashedStateCookieName = "apribotOauth2State"

tokenCookieName :: Text
tokenCookieName = "apribotToken"

-- * Generating RedditEnv values to use

-- | Retrieve token-identifier cookie, and thus the token from the database, if
-- the user has one.
retrieveRedditEnv :: ST.ActionT TL.Text (App IO) (Maybe RedditEnv)
retrieveRedditEnv = do
  userAgent <- asks cfgUserAgent
  clientId <- asks cfgRedditFrontendId
  clientSecret <- asks cfgRedditFrontendSecret

  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie of
    Nothing -> pure Nothing
    Just sessionId -> do
      maybeToken <- lift $ getToken sessionId
      case maybeToken of
        Nothing -> pure Nothing
        Just t -> do
          -- Check if the token is expired
          now <- liftIO getCurrentTime
          if now > tokenExpiresAt t
            then do
              -- If it is, refresh the token, update the database, and create a new redditEnv from it
              rt <- liftIO $ refresh clientId clientSecret userAgent t
              lift $ atomically $ T.putStrLn "refreshing token"
              lift $ updateToken sessionId rt
              Just <$> liftIO (mkEnvFromToken rt userAgent)
            else -- If it's still valid, create a new redditEnv from this token
              Just <$> liftIO (mkEnvFromToken t userAgent)

-- | Make a new RedditEnv by requesting a token. This is to be used on the
-- redirect URI and assumes that the user has been redirected to here after
-- granting access on Reddit (i.e. the URI query params contains an
-- authorisation code which can be used to request an access token). If the
-- params are not present then this redirects the user back to "/".
makeNewRedditEnv :: ST.ActionT TL.Text (App IO) ()
makeNewRedditEnv = do
  cfg <- ask
  -- Retrieve stored state from the cookie
  hashedState <- fmap PasswordHash <$> SC.getCookie hashedStateCookieName
  -- Get the authentication code and state from the URI query parameters, and
  -- check that they're correct
  authCodeMaybe <- (Just <$> ST.param "code") `ST.rescue` const (pure Nothing)
  returnedState <- (Just <$> ST.param "state") `ST.rescue` const (pure Nothing)
  case (authCodeMaybe, liftA2 checkPassword (mkPassword <$> returnedState) hashedState) of
    (Just authCode, Just PasswordCheckSuccess) -> do
      -- Get rid of the state
      SC.deleteCookie hashedStateCookieName
      -- Authenticate using the code that we just got
      let creds =
            CodeGrantCredentials
              { codeGrantClientId = cfgRedditFrontendId cfg,
                codeGrantClientSecret = cfgRedditFrontendSecret cfg,
                codeGrantRedirectUri = cfgRedirectUri cfg,
                codeGrantCode = authCode
              }
      env <- liftIO $ authenticate creds (cfgUserAgent cfg)
      t <- liftIO $ getTokenFromEnv env
      -- Generate a random session ID for the user and store it in a cookie
      sessionId <- liftIO $ randomText 60
      let sessionCookie = SC.makeSimpleCookie tokenCookieName sessionId
      SC.setCookie $
        sessionCookie
          { C.setCookieMaxAge = Just $ secondsToDiffTime $ 2 * 60 * 60 * 24 * 365, -- 2 years
            C.setCookieHttpOnly = True,
            C.setCookieSameSite = Just C.sameSiteLax,
            C.setCookieSecure = True
          }
      -- Store the token in the database using this session ID
      lift $ addToken sessionId t
    _ -> do
      ST.redirect "auth_error"

-- | Deletes all cookies and the corresponding token from the database (if it
-- exists)
cleanup :: ST.ActionT TL.Text (App IO) ()
cleanup = do
  maybeCookie <- SC.getCookie tokenCookieName
  case maybeCookie of
    Just cookie -> do
      lift $ removeToken cookie
      SC.deleteCookie tokenCookieName
      SC.deleteCookie hashedStateCookieName
    Nothing -> pure ()

-- * HTML helpers

data NavigationLink = Home | Contribute | YourVotes | Privacy | Logout

getRouteDesc :: NavigationLink -> (Text, Text)
getRouteDesc Home = ("/", "home")
getRouteDesc Contribute = ("/contribute", "contribute")
getRouteDesc YourVotes = ("/your_votes", "view your votes")
getRouteDesc Privacy = ("/privacy", "privacy")
getRouteDesc Logout = ("/logout", "logout")

mkLinkHtml :: NavigationLink -> Html ()
mkLinkHtml link =
  let (route, desc) = getRouteDesc link
   in a_ [href_ route] (toHtml desc)

navigation :: [NavigationLink] -> Html ()
navigation links = do
  p_ $ i_ $ do
    "("
    sequence_ . intersperse " — " $ map mkLinkHtml links
    ")"

-- * HTML that is served

data WithJS = NoJS | WithJS2Sec | WithJS0p5Sec

trustedUsers :: [Text]
trustedUsers = ["is_a_togekiss", "againpedro", "JBSouls"]

-- | Reusable <head> element lead element for HTML page
headHtml :: Maybe Text -> WithJS -> Html ()
headHtml titleExtra withJS = do
  head_ $ do
    title_ $ toHtml (maybe "ApriBot" ("ApriBot :: " <>) titleExtra)
    link_ [rel_ "stylesheet", href_ "static/styles.css"]
    link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "static/apple-touch-icon.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "static/favicon-32x32.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "static/favicon-16x16.png"]
    link_ [rel_ "manifest", href_ "static/site.webmanifest"]
    case withJS of
      NoJS -> mempty
      WithJS2Sec -> script_ [src_ "static/enableButton2.js", type_ "module"] ("" :: Text)
      WithJS0p5Sec -> script_ [src_ "static/enableButton0p5.js", type_ "module"] ("" :: Text)

-- | Error HTML
errorHtml :: Html ()
errorHtml = doctypehtml_ $ do
  headHtml (Just "Error") NoJS
  body_ $ do
    h1_ "An error occurred :("
    p_ $ do
      "Sorry about that. You can go back to the "
      a_ [href_ "/"] "home page"
      "."

-- | HTML for the main page
mainHtml :: App IO (Html ())
mainHtml = do
  hits <- getLatestHits 50
  nonhits <- getLatestNonHits 50
  n <- getTotalMLAssignedRows
  m <- getTotalMLAssignedHits
  let tblHeader :: Html ()
      tblHeader =
        tr_ $ mapM_ (th_ . toHtml) ["Post ID" :: Text, "Time (UTC)", "Submitter", "Flair", "Title"]
  let tblRow :: (Text, Text, Text, Text, UTCTime, Maybe Text) -> Html ()
      tblRow (pid, purl, ptitle, psubmitter, ptime, pflair) =
        tr_ $ do
          mapM_
            td_
            [ code_ (toHtml pid),
              toHtml (show ptime),
              toHtml ("/u/" <> psubmitter),
              toHtml (fromMaybe "" pflair)
            ]
          td_ [class_ "post-title"] $ a_ [href_ purl] $ toHtmlRaw $ sanitizeBalance ptitle

  pure $ doctypehtml_ $ do
    headHtml Nothing NoJS
    body_ $ main_ $ do
      h1_ "ApriBot"
      div_ [class_ "prose"] $ do
        p_ $ do
          "ApriBot monitors new posts on the "
          a_ [href_ "https://reddit.com/r/pokemontrades"] "/r/pokemontrades"
          " subreddit and identifies potential Aprimon-related threads, using a machine learning algorithm trained on manually labelled /r/pokemontrades posts."
        p_ $ do
          toHtml (printf "So far, ApriBot's new algorithm has processed a total of %d posts," n :: String)
          toHtml (printf " of which %d were hits (%.2f%%)." m (100 * fromIntegral m / fromIntegral n :: Double) :: String)
          " This page shows you the most recent 50 hits and non-hits, ordered by most recent first."
        p_ $ do
          "ApriBot is implemented with Haskell and SQLite; the ML bits are written in Python."
          " If you have any questions about ApriBot, feel free to get in touch with me, either via "
          a_ [href_ "https://github.com/penelopeysm/apribot"] "GitHub"
          " or "
          a_ [href_ "https://reddit.com/u/is_a_togekiss"] "Reddit"
          "."
        p_ $ do
          b_ "While the current algorithm has pretty decent performance, I might be able to make it even better in the future with more data."
          " If you are interested and have a few minutes to spare, do head over to the "
          a_ [href_ "/contribute"] "contribute page"
          "—I will be very grateful!"
      h2_ "Hits"
      if null hits
        then p_ "None so far!"
        else table_ $ do
          thead_ tblHeader
          tbody_ $ mapM_ tblRow (take 50 hits)
      h2_ "Non-hits"
      if null nonhits
        then p_ "None so far!"
        else table_ $ do
          thead_ tblHeader
          tbody_ $ mapM_ tblRow (take 50 nonhits)

logoutHtml :: Html ()
logoutHtml = doctypehtml_ $ do
  headHtml (Just "Logged out") NoJS
  body_ $ main_ $ do
    h1_ "Logged out"
    navigation [Home, Contribute, Privacy]
    p_ "You have been logged out. Thank you so much for your time!"

privacyHtml :: Html ()
privacyHtml = doctypehtml_ $ do
  headHtml (Just "Privacy") NoJS
  body_ $ main_ $ do
    h1_ "Privacy"
    navigation [Home, Contribute]
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
authErrorHtml = doctypehtml_ $ do
  headHtml (Just "Error") NoJS
  body_ $ main_ $ do
    h1_ "Authentication error :("
    p_ $ do
      "Sorry! There was an error logging you in. "
      "This might be because you denied ApriBot access, or you took too long to log in (you have to do so within 10 minutes of opening the page)."
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contribErrorHtml :: Html ()
contribErrorHtml = doctypehtml_ $ do
  headHtml (Just "Error") NoJS
  body_ $ main_ $ do
    h1_ "Form submission error :("
    p_ $ do
      "Sorry! There was an error recording your vote. "
    p_ $ a_ [href_ "/contribute"] "Please try again, and let me know if it still doesn't work."

contributingLoggedOutHtml' :: App IO (Html ())
contributingLoggedOutHtml' = do
  totalPosts <- getTotalRows
  labelledPosts <- getTotalNumberLabelled
  pure $ doctypehtml_ $ do
    headHtml (Just "Contributing") NoJS
    body_ $ main_ $ do
      h1_ "Contribute"
      navigation [Home, Privacy]
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

contributingLoggedInHtml' :: Text -> App IO (Html ())
contributingLoggedInHtml' username = do
  totalPosts <- getTotalRows
  labelledPosts <- getTotalNumberLabelled
  labelledPostsByUser <- getNumberLabelledBy username
  nextPost <- getNextUnlabelledPost

  pure $ doctypehtml_ $ do
    headHtml (Just "Contributing") (if username `elem` trustedUsers then WithJS0p5Sec else WithJS2Sec)
    body_ $ main_ $ do
      h1_ "Contribute"
      navigation [Home, YourVotes, Privacy, Logout]
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
              span_ [class_ "boxed-flair"] $ toHtml $ fromMaybe "" postFlair
            ul_ $ do
              li_ $ toHtml (printf "Submitted by /u/%s at %s UTC" postSubmitter (show postTime) :: String)
              li_ $ do
                a_ [href_ postUrl] "Link to original Reddit post"
            if T.null (T.strip postBody)
              then p_ "<empty post body>"
              else
                div_ [class_ "post-body"] $
                  toHtmlRaw $
                    commonmarkToHtml [optSmart] [extTable, extStrikethrough] postBody

yourVotesHtml' :: Text -> App IO (Html ())
yourVotesHtml' username = do
  votes <- getLastNVotesBy 100 username
  totalLabelledByUser <- getNumberLabelledBy username
  let makeTableRow :: (Text, Text, Text, Text, Bool) -> Html ()
      makeTableRow (postId, postTitle, postUrl, postSubmitter, vote) =
        tr_ $ do
          td_ $ code_ $ toHtml postId
          td_ $ a_ [href_ postUrl] (toHtmlRaw $ sanitizeBalance postTitle)
          td_ $ toHtml $ "/u/" <> postSubmitter
          td_ $ if vote then "Yes" else "No"

  pure $ do
    headHtml (Just "Your votes") NoJS
    body_ $ main_ $ do
      h1_ "Your votes"
      navigation [Home, Contribute, Privacy, Logout]
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
                mapM_ th_ ["Post ID", "Post title", "Submitter", "Your vote"]
            tbody_ $ do
              mapM_ makeTableRow votes

-- | Serve static file
static :: FilePath -> TL.Text -> ST.ActionT TL.Text (App IO) ()
static path mimeType = do
  staticDir <- asks cfgStaticDir
  ST.setHeader "Content-Type" mimeType
  ST.file (staticDir <> "/" <> path)

-- * The web app

web :: App IO ()
web = do
  cfg <- ask
  atomically $ printf "Launching web server on port %d...\n" (cfgPort cfg)

  ST.scottyT (cfgPort cfg) (runAppWith cfg) $ do
    -- everything inside here is ~ ScottyT TL.Text (App IO) ()
    let serve = ST.html . renderText
    let serve' html' = do
          html <- liftIO $ runAppWith cfg html'
          serve html

    ST.defaultHandler $ \e -> do
      lift $ atomically $ printf "Scotty caught exception: <%s>\n" (show e)
      serve errorHtml

    ST.get "/static/styles.css" $ do
      static "styles.css" "text/css"
    ST.get "/static/site.webmanifest" $ do
      static "site.webmanifest" "application/manifest+json"
    ST.get "/static/favicon.ico" $ do
      static "favicon.ico" "image/x-icon"
    ST.get "/static/favicon-16x16.png" $ do
      static "favicon-16x16.png" "image/x-png"
    ST.get "/static/favicon-32x32.png" $ do
      static "favicon-32x32.png" "image/x-png"
    ST.get "/static/apple-touch-icon.png" $ do
      static "apple-touch-icon.png" "image/x-png"
    ST.get "/static/android-chrome-192x192.png" $ do
      static "android-chrome-192x192.png" "image/x-png"
    ST.get "/static/android-chrome-512x512.png" $ do
      static "android-chrome-512x512.png" "image/x-png"
    ST.get "/static/enableButton2.js" $ do
      static "enableButton2.js" "text/javascript"
    ST.get "/static/enableButton0p5.js" $ do
      static "enableButton0p5.js" "text/javascript"

    ST.get "/" $ do
      lift mainHtml >>= serve

    ST.get "/authorised" $ do
      makeNewRedditEnv
      ST.redirect "/contribute"

    ST.get "/auth_error" $ do
      serve authErrorHtml

    ST.get "/contrib_error" $ do
      serve contribErrorHtml

    ST.get "/login" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        Just _ -> ST.redirect "/contribute"
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
          clientId <- asks cfgRedditFrontendId
          redirectUri <- asks cfgRedirectUri
          ST.redirect $
            fromStrict $
              mkRedditAuthURL
                False
                AuthUrlParams
                  { authUrlClientID = clientId,
                    authUrlState = state,
                    authUrlRedirectUri = redirectUri,
                    authUrlDuration = Permanent,
                    authUrlScopes = Set.singleton ScopeIdentity
                  }

    ST.get "/logout" $ do
      maybeCookie <- SC.getCookie tokenCookieName
      case maybeCookie of
        Nothing -> ST.redirect "/"
        Just _ -> cleanup >> serve logoutHtml

    ST.post "/contribute" $ do
      mPostId :: Maybe Text <- (Just <$> ST.param "id") `ST.rescue` const (pure Nothing)
      mVoter :: Maybe Text <- (Just <$> ST.param "username") `ST.rescue` const (pure Nothing)
      mVote :: Maybe Int <- (Just <$> ST.param "vote") `ST.rescue` const (pure Nothing)
      case (mPostId, mVoter, mVote) of
        (Just postId, Just voter, Just vote) ->
          if vote /= 0 && vote /= 1
            then ST.redirect "/contribute"
            else do
              lift $ addVote postId voter (vote == 1)
              hit <- lift $ wasHit postId
              when (not hit && vote == 1) $ lift $ do
                atomically $ T.putStrLn ("Notifying about false negative " <> postId)
                notifyDiscord (NotifyPostById (PostID postId))
              when (hit && vote == 0) $ lift $ do
                atomically $ T.putStrLn ("Removing false positive " <> postId)
                notifyDiscord (UnnotifyPostById (PostID postId))
              ST.redirect "/contribute"
        _ -> ST.redirect "/contrib_error"

    ST.get "/contribute" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        Nothing -> serve' contributingLoggedOutHtml'
        Just env -> do
          usernameEither <- liftIO $ try $ runRedditT env (accountUsername <$> myAccount)
          case usernameEither of
            Left e -> do
              cleanup
              ST.raise (TL.pack $ show (e :: SomeException))
            Right username -> do
              serve' (contributingLoggedInHtml' username)

    ST.get "/privacy" $ do
      serve privacyHtml

    ST.get "/your_votes" $ do
      maybeEnv <- retrieveRedditEnv
      case maybeEnv of
        Nothing -> ST.redirect "/contribute"
        Just env -> do
          usernameEither <- liftIO $ try $ runRedditT env (accountUsername <$> myAccount)
          case usernameEither of
            Left e -> do
              cleanup
              ST.raise (TL.pack $ show (e :: SomeException))
            Right username -> do
              serve' (yourVotesHtml' username)
