{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web (web) where

import CMarkGFM
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database
import DiscordBot (notifyDiscord)
import GHC.Generics
import Lucid
import Network.HTTP.Types.Status
import Reddit
import Reddit.Auth (Token (..), refresh)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Printf (printf)
import Trans
import Utils
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

newtype RedditLoggedInResponse = RedditLoggedInResponse
  {sessionId :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

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

-- * HTML that is served (TODO: PHASE OUT)

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

-- * The web app

-- | /api/total_assigned
data TotalAssignedResponse = TotalAssignedResponse
  {rows :: Int, hits :: Int}
  deriving (Generic, Show, ToJSON)

data MainPagePostDetails = MainPagePostDetails
  { post_id :: Text,
    post_url :: Text,
    post_title :: Text,
    post_submitter :: Text,
    post_time :: UTCTime,
    post_flair :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON MainPagePostDetails where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = drop 5}

-- | /api/ml_stats
data MLStatsResponse = MLStatsResponse
  {seen :: Int, labelled :: Int}
  deriving (Generic, Show, ToJSON)

newtype BackendError = BackendError
  { error :: Text
  }
  deriving (Generic, Show, ToJSON)

newtype CodeParam = CodeParam
  { code :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype StateParam = StateParam
  { state :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

web :: App IO ()
web = do
  cfg <- ask
  atomically $ printf "Launching web server on port %d...\n" (cfgPort cfg)

  let x :: ST.ScottyT TL.Text (App IO) ()
      x = do
        -- everything inside here is ~ ScottyT TL.Text (App IO) ()
        ST.get (ST.regex "^.*$") $ do
          ST.html $ renderText $ p_ "ApriBot's web frontend is currently being redeveloped; please check back in a day or so."

        -- -- Backend API
        -- ST.get "/api/total_assigned" $ do
        --   totalRows <- lift getTotalMLAssignedRows
        --   totalHits <- lift getTotalMLAssignedHits
        --   ST.json $ TotalAssignedResponse totalRows totalHits

        -- ST.get "/api/hits" $ do
        --   nPosts :: Int <- ST.param "limit" `ST.rescue` const (pure 50)
        --   posts <- lift $ getLatestHits nPosts
        --   -- TODO: Ugly. The conversion should be done in getLatestHits
        --   ST.json $ map (\(a, b, c, d, e, f) -> MainPagePostDetails a b c d e f) posts

        -- ST.get "/api/nonhits" $ do
        --   nPosts :: Int <- ST.param "limit" `ST.rescue` const (pure 50)
        --   posts <- lift $ getLatestNonHits nPosts
        --   -- TODO: Ugly. The conversion should be done in getLatestHits
        --   ST.json $ map (\(a, b, c, d, e, f) -> MainPagePostDetails a b c d e f) posts

        -- ST.get "/api/ml_stats" $ do
        --   totalPosts <- lift getTotalRows
        --   labelledPosts <- lift getTotalNumberLabelled
        --   ST.json $ MLStatsResponse totalPosts labelledPosts

        -- ST.post "/api/login_username" $ do
        --   let clientId = cfgRedditFrontendId cfg
        --       clientSecret = cfgRedditFrontendSecret cfg
        --       userAgent = cfgUserAgent cfg
        --   maybeSessionId :: Maybe RedditLoggedInResponse <- decode <$> ST.body
        --   case maybeSessionId of
        --     Nothing -> do
        --       ST.status status400
        --       ST.json $ BackendError "No session ID provided"
        --     Just (RedditLoggedInResponse sessionId) -> do
        --       maybeToken <- lift $ getToken sessionId
        --       case maybeToken of
        --         Nothing -> do
        --           ST.status status401
        --           ST.json $ BackendError "No token found for this session ID"
        --         Just t -> do
        --           -- Check if the token is expired. If so, refresh the token and update
        --           -- the database.
        --           activeToken <- do
        --             now <- liftIO getCurrentTime
        --             if now > tokenExpiresAt t
        --               then do
        --                 rt <- liftIO $ refresh clientId clientSecret userAgent t
        --                 lift $ atomically $ T.putStrLn "refreshing token"
        --                 lift $ updateToken sessionId rt
        --                 pure rt
        --               else pure t
        --           -- Get the logged in user's name using the token
        --           env <- liftIO (mkEnvFromToken activeToken userAgent)
        --           usernameEither <- liftIO $ try $ runRedditT env (accountUsername <$> myAccount)
        --           case usernameEither of
        --             Left (e :: SomeException) -> do
        --               lift $ atomically $ print e
        --               ST.status status401
        --               ST.json $ BackendError "Could not retrieve Reddit username from token"
        --             Right username -> do
        --               ST.json $ object ["username" .= username]

        -- ST.post "/api/make_reddit_token" $ do
        --   maybeAuthCode :: Maybe CodeParam <- decode <$> ST.body
        --   case maybeAuthCode of
        --     Nothing -> do
        --       ST.status status400
        --       ST.json $ BackendError "No code provided"
        --     Just (CodeParam authCode) -> do
        --       -- Authenticate using the code that we just got
        --       let creds =
        --             CodeGrantCredentials
        --               { codeGrantClientId = cfgRedditFrontendId cfg,
        --                 codeGrantClientSecret = cfgRedditFrontendSecret cfg,
        --                 codeGrantRedirectUri = cfgRedirectUri cfg,
        --                 codeGrantCode = authCode
        --               }
        --       env <- liftIO $ authenticate creds (cfgUserAgent cfg)
        --       t <- liftIO $ getTokenFromEnv env
        --       -- Generate a random session ID for the user and store the token in the
        --       -- database using this
        --       sessionId <- liftIO $ randomText 60
        --       lift $ addToken sessionId t
        --       -- Return the session ID
        --       ST.json $ RedditLoggedInResponse sessionId

        -- ST.post "/api/get_login_url" $ do
        --   maybeState :: Maybe StateParam <- decode <$> ST.body
        --   case maybeState of
        --     Nothing -> do
        --       ST.status status400
        --       ST.json $ BackendError "No state parameter provided"
        --     Just (StateParam state) -> do
        --       let clientId = cfgRedditFrontendId cfg
        --           redirectUri = cfgRedirectUri cfg
        --           url =
        --             mkRedditAuthURL
        --               False
        --               AuthUrlParams
        --                 { authUrlClientID = clientId,
        --                   authUrlState = state,
        --                   authUrlRedirectUri = redirectUri,
        --                   authUrlDuration = Permanent,
        --                   authUrlScopes = Set.singleton ScopeIdentity
        --                 }
        --       ST.json $ object ["url" .= url]

        -- ST.post "/api/logout" $ do
        --   maybeSessionId :: Maybe RedditLoggedInResponse <- decode <$> ST.body
        --   case maybeSessionId of
        --     Nothing -> do
        --       ST.status status400
        --       ST.json $ BackendError "No session ID provided"
        --     Just (RedditLoggedInResponse sessionId) -> do
        --       lift $ removeToken sessionId
        --       ST.json $ object ["success" .= True]

        -- ST.post "/api/contribute" $ do
        --   mPostId :: Maybe Text <- (Just <$> ST.param "id") `ST.rescue` const (pure Nothing)
        --   mVoter :: Maybe Text <- (Just <$> ST.param "username") `ST.rescue` const (pure Nothing)
        --   mVote :: Maybe Int <- (Just <$> ST.param "vote") `ST.rescue` const (pure Nothing)
        --   case (mPostId, mVoter, mVote) of
        --     (Just postId, Just voter, Just vote) ->
        --       if vote /= 0 && vote /= 1
        --         then ST.redirect "/contribute"
        --         else do
        --           lift $ addVote postId voter (vote == 1)
        --           hit <- lift $ wasHit postId
        --           when (not hit && vote == 1) $ lift $ do
        --             atomically $ T.putStrLn ("Notifying about false negative " <> postId)
        --             notifyDiscord (NotifyPostById (PostID postId))
        --           when (hit && vote == 0) $ lift $ do
        --             atomically $ T.putStrLn ("Removing false positive " <> postId)
        --             notifyDiscord (UnnotifyPostById (PostID postId))
        --           ST.redirect "/contribute"
        --     _ -> ST.redirect "/contrib_error"

  ST.scottyT (cfgPort cfg) (runAppWith cfg) x
