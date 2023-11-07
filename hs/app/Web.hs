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
import Data.Time.Clock (getCurrentTime)
import qualified Database as DB
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

data VoteChangeParam = VoteChangeParam
  { vote_post_id :: Text,
    vote_username :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

web :: App IO ()
web = do
  cfg <- ask
  atomically $ printf "Launching web server on port %d...\n" (cfgPort cfg)

  ST.scottyT (cfgPort cfg) (runAppWith cfg) $ do
    -- everything inside here is ~ ScottyT TL.Text (App IO) ()
    -- need the type application to suppress ambiguous type warning
    ST.post @TL.Text "/api/login_username" $ do
      let clientId = cfgRedditFrontendId cfg
          clientSecret = cfgRedditFrontendSecret cfg
          userAgent = cfgUserAgent cfg
      maybeSessionId :: Maybe RedditLoggedInResponse <- decode <$> ST.body
      case maybeSessionId of
        Nothing -> do
          ST.status status400
          ST.json $ BackendError "No session ID provided"
        Just (RedditLoggedInResponse sessionId) -> do
          maybeToken <- lift $ DB.getToken sessionId
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
                    lift $ DB.updateToken sessionId rt
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
          lift $ DB.addToken sessionId t
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
          lift $ DB.removeToken sessionId
          ST.json $ object ["success" .= True]

    ST.post "/api/contribute" $ do
      maybeContribute :: Maybe ContributeParam <- decode <$> ST.body
      case maybeContribute of
        Just (ContributeParam postId voter vote) -> do
          if vote /= 0 && vote /= 1
            then ST.json $ BackendError "invalid_vote"
            else do
              lift $ DB.addVote postId voter (vote == 1)
              hit <- lift $ DB.wasHit postId
              when (not hit && vote == 1) $ lift $ do
                atomically $ T.putStrLn ("Notifying about false negative " <> postId)
                notifyDiscord (NotifyPostById (PostID postId))
              when (hit && vote == 0) $ lift $ do
                atomically $ T.putStrLn ("Removing false positive " <> postId)
                notifyDiscord (UnnotifyPostById (PostID postId))
              ST.json $ object ["success" .= True]
        Nothing -> ST.json $ BackendError "invalid_vote"
