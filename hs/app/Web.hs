{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Web (web) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Set as Set
import Data.Text (Text, pack)
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

newtype RedditLoggedInResponse = RedditLoggedInResponse
  {sessionId :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

-- | /api/total_assigned
data TotalAssignedResponse = TotalAssignedResponse
  {rows :: Int, hits :: Int}
  deriving (Generic, Show, ToJSON)

data RedditPostDetails = RedditPostDetails
  { post_id :: Text,
    post_url :: Text,
    post_title :: Text,
    post_body :: Text,
    post_submitter :: Text,
    post_time :: UTCTime,
    post_flair :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON RedditPostDetails where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = drop 5}

-- | /api/ml_stats
data MLStatsResponse = MLStatsResponse
  { seen :: Int,
    labelled :: Int,
    need_not_label :: Int
  }
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

data ContributeParam = ContributeParam
  { contrib_post_id :: Text,
    contrib_username :: Text,
    contrib_vote :: Int
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
      ST.json $ map (\(a, b, c, d, e, f) -> RedditPostDetails a b c "" d e f) posts

    ST.get "/api/nonhits" $ do
      nPosts :: Int <- ST.param "limit" `ST.rescue` const (pure 50)
      posts <- lift $ getLatestNonHits nPosts
      -- TODO: Ugly. The conversion should be done in getLatestHits
      ST.json $ map (\(a, b, c, d, e, f) -> RedditPostDetails a b c "" d e f) posts

    ST.get "/api/ml_stats" $ do
      totalPosts <- lift getTotalRows
      labelledPosts <- lift getTotalNumberLabelled
      needNotLabel <- lift getTotalNumberNeedNotLabel
      ST.json $ MLStatsResponse totalPosts labelledPosts needNotLabel

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
      maybeContribute :: Maybe ContributeParam <- decode <$> ST.body
      case maybeContribute of
        Just (ContributeParam postId voter vote) -> do
          if vote /= 0 && vote /= 1
            then ST.json $ BackendError "invalid_vote"
            else do
              lift $ addVote postId voter (vote == 1)
              hit <- lift $ wasHit postId
              when (not hit && vote == 1) $ lift $ do
                atomically $ T.putStrLn ("Notifying about false negative " <> postId)
                notifyDiscord (NotifyPostById (PostID postId))
              when (hit && vote == 0) $ lift $ do
                atomically $ T.putStrLn ("Removing false positive " <> postId)
                notifyDiscord (UnnotifyPostById (PostID postId))
              ST.json $ object ["success" .= True]
        Nothing -> ST.json $ BackendError "invalid_vote"

    ST.get "/api/user_stats" $ do
      username :: Text <- ST.param "username"
      n :: Int <- ST.param "limit" `ST.rescue` const (pure 100)
      userStats <- lift $ getUserStats n username
      ST.json userStats

    ST.get "/api/next_unlabelled" $ do
      nextPost <- lift getNextUnlabelledPost
      case nextPost of
        Nothing -> ST.json $ BackendError "no_posts_needing_review"
        Just (a, b, c, d, e, f, g) ->
          ST.json $
            object
              [ "error" .= ("" :: Text),
                "post" .= RedditPostDetails a b c d e f g
              ]
