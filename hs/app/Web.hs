{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Web (web) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database
import DiscordBot (notifyDiscord)
import GHC.Generics
import Network.HTTP.Types.Status
import Reddit
import Reddit.Auth (Token (..), refresh)
import Text.Printf (printf)
import Trans
import Utils
import qualified Web.Scotty.Trans as ST

-- contributingLoggedInHtml' :: Text -> App IO (Html ())
-- contributingLoggedInHtml' username = do
--   pure $ doctypehtml_ $ do
--     body_ $ main_ $ do
--       h1_ "Contribute"
--       p_ $ b_ $ do
--         "You are now logged in as: /u/"
--         toHtml username
--       p_ $ do
--         "Right now, out of a total of "
--         toHtml (show totalPosts)
--         " posts, "
--         toHtml (show labelledPosts)
--         " have been labelled. "
--         if labelledPostsByUser > 0
--           then do
--             toHtml $ show labelledPostsByUser
--             " of these were by you. Thank you so much! <3"
--           else ""
--         case nextPost of
--           -- This is very optimistic...
--           Nothing -> do
--             hr_ []
--             p_ "There are no more unlabelled posts. Please check back again tomorrow!"
--           Just (postId, postUrl, postTitle, postBody, postSubmitter, postTime, postFlair) -> do
--             div_ [class_ "form-container"] $ do
--               form_ [class_ "aprimon-question", action_ "/contribute", method_ "post"] $ do
--                 span_ $ b_ "Is the post below offering, or looking for, non-shiny breedable Aprimon?"
--                 input_ [type_ "hidden", name_ "id", value_ postId]
--                 input_ [type_ "hidden", name_ "username", value_ username]
--                 div_ [id_ "button-container"] $ do
--                   button_ [type_ "submit", name_ "vote", value_ "1", disabled_ ""] "✅ Yes"
--                   button_ [type_ "submit", name_ "vote", value_ "0", disabled_ ""] "❌ No"
--                   button_ [type_ "submit", name_ "vote", value_ "2", disabled_ ""] "⏭️ Skip"
--             div_ $ do
--               span_ [class_ "title"] $ toHtmlRaw $ sanitizeBalance postTitle
--               span_ [class_ "boxed-flair"] $ toHtml $ fromMaybe "" postFlair
--             ul_ $ do
--               li_ $ toHtml (printf "Submitted by /u/%s at %s UTC" postSubmitter (show postTime) :: String)
--               li_ $ do
--                 a_ [href_ postUrl] "Link to original Reddit post"
--             if T.null (T.strip postBody)
--               then p_ "<empty post body>"
--               else
--                 div_ [class_ "post-body"] $
--                   toHtmlRaw $
--                     commonmarkToHtml [optSmart] [extTable, extStrikethrough] postBody

-- * The web app

newtype RedditLoggedInResponse = RedditLoggedInResponse
  {sessionId :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

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

  ST.scottyT (cfgPort cfg) (runAppWith cfg) $ do
    -- everything inside here is ~ ScottyT TL.Text (App IO) ()
    -- need the type application to suppress ambiguous type warning
    ST.get @TL.Text "/api/total_assigned" $ do
      totalRows <- lift getTotalMLAssignedRows
      totalHits <- lift getTotalMLAssignedHits
      ST.json $ TotalAssignedResponse totalRows totalHits

    ST.get "/api/names" $ do
      lift getAllNames >>= ST.json

    ST.get "/api/hits" $ do
      nPosts :: Int <- ST.param "limit" `ST.rescue` const (pure 50)
      posts <- lift $ getLatestHits nPosts
      -- TODO: Ugly. The conversion should be done in getLatestHits
      ST.json $ map (\(a, b, c, d, e, f) -> MainPagePostDetails a b c d e f) posts

    ST.get "/api/nonhits" $ do
      nPosts :: Int <- ST.param "limit" `ST.rescue` const (pure 50)
      posts <- lift $ getLatestNonHits nPosts
      -- TODO: Ugly. The conversion should be done in getLatestHits
      ST.json $ map (\(a, b, c, d, e, f) -> MainPagePostDetails a b c d e f) posts

    ST.get "/api/ml_stats" $ do
      totalPosts <- lift getTotalRows
      labelledPosts <- lift getTotalNumberLabelled
      ST.json $ MLStatsResponse totalPosts labelledPosts

    ST.post "/api/login_username" $ do
      let clientId = cfgRedditFrontendId cfg
          clientSecret = cfgRedditFrontendSecret cfg
          userAgent = cfgUserAgent cfg
      maybeSessionId :: Maybe RedditLoggedInResponse <- decode <$> ST.body
      case maybeSessionId of
        Nothing -> do
          ST.status status400
          ST.json $ BackendError "No session ID provided"
        Just (RedditLoggedInResponse sessionId) -> do
          maybeToken <- lift $ getToken sessionId
          case maybeToken of
            Nothing -> do
              ST.status status401
              ST.json $ BackendError "No token found for this session ID"
            Just t -> do
              -- Check if the token is expired. If so, refresh the token and update
              -- the database.
              activeToken <- do
                now <- liftIO getCurrentTime
                if now > tokenExpiresAt t
                  then do
                    rt <- liftIO $ refresh clientId clientSecret userAgent t
                    lift $ atomically $ T.putStrLn "refreshing token"
                    lift $ updateToken sessionId rt
                    pure rt
                  else pure t
              -- Get the logged in user's name using the token
              env <- liftIO (mkEnvFromToken activeToken userAgent)
              usernameEither <- liftIO $ try $ runRedditT env (accountUsername <$> myAccount)
              case usernameEither of
                Left (e :: SomeException) -> do
                  lift $ atomically $ print e
                  ST.status status401
                  ST.json $ BackendError "Could not retrieve Reddit username from token"
                Right username -> do
                  ST.json $ object ["username" .= username]

    ST.post "/api/make_reddit_token" $ do
      maybeAuthCode :: Maybe CodeParam <- decode <$> ST.body
      case maybeAuthCode of
        Nothing -> do
          ST.status status400
          ST.json $ BackendError "No code provided"
        Just (CodeParam authCode) -> do
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
          -- Generate a random session ID for the user and store the token in the
          -- database using this
          sessionId <- liftIO $ randomText 60
          lift $ addToken sessionId t
          -- Return the session ID
          ST.json $ RedditLoggedInResponse sessionId

    ST.post "/api/get_login_url" $ do
      maybeState :: Maybe StateParam <- decode <$> ST.body
      case maybeState of
        Nothing -> do
          ST.status status400
          ST.json $ BackendError "No state parameter provided"
        Just (StateParam state) -> do
          let clientId = cfgRedditFrontendId cfg
              redirectUri = cfgRedirectUri cfg
              url =
                mkRedditAuthURL
                  False
                  AuthUrlParams
                    { authUrlClientID = clientId,
                      authUrlState = state,
                      authUrlRedirectUri = redirectUri,
                      authUrlDuration = Permanent,
                      authUrlScopes = Set.singleton ScopeIdentity
                    }
          ST.json $ object ["url" .= url]

    ST.post "/api/logout" $ do
      maybeSessionId :: Maybe RedditLoggedInResponse <- decode <$> ST.body
      case maybeSessionId of
        Nothing -> do
          ST.status status400
          ST.json $ BackendError "No session ID provided"
        Just (RedditLoggedInResponse sessionId) -> do
          lift $ removeToken sessionId
          ST.json $ object ["success" .= True]

    ST.post "/api/contribute" $ do
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

    ST.get "/api/user_stats" $ do
      username :: Text <- ST.param "username"
      n :: Int <- ST.param "limit" `ST.rescue` const (pure 100)
      userStats <- lift $ getUserStats n username
      ST.json userStats
