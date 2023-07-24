module Config (Config (..), getConfig, NotifyEvent (..)) where

import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Types
import Paths_apribot (getDataFileName)
import Reddit (Post, ID)
import System.Environment (getEnv, lookupEnv)

data NotifyEvent
  = NotifyPost Post
  | NotifyPostById (ID Post)
  deriving (Show)

data Config = Config
  { -- | Path to the posts.db SQLite database file.
    cfgPostsDbPath :: FilePath,
    -- | Path to the tokens.db SQLite database file.
    cfgTokensDbPath :: FilePath,
    -- | Path to web app static directory
    cfgStaticDir :: FilePath,
    -- | Discord channel to post /r/pokemontrades posts to.
    cfgPtrChannelId :: ChannelId,
    -- | Discord channel to post /r/bankballexchange posts to.
    cfgBbeChannelId :: ChannelId,
    -- | Port to listen on.
    cfgPort :: Int,
    -- | User agent to use for Reddit API requests.
    cfgUserAgent :: Text,
    -- | Redirect URI for OAuth2.
    cfgRedirectUri :: Text,
    -- | Path to the Python classifier script.
    cfgClassifierPath :: FilePath,
    -- | Discord token (set via $DISCORD_APRIBOT_TOKEN)
    cfgDiscordToken :: Text,
    -- | Reddit ID for crawler (set via $REDDIT_ID)
    cfgRedditId :: Text,
    -- | Reddit secret for crawler (set via $REDDIT_SECRET)
    cfgRedditSecret :: Text,
    -- | Reddit ID for web app (set via $REDDIT_FE_ID)
    cfgRedditFrontendId :: Text,
    -- | Reddit secret for web app (set via $REDDIT_FE_SECRET)
    cfgRedditFrontendSecret :: Text,
    -- | Reddit username for the bot (/u/ApriBot, but set via $REDDIT_USERNAME)
    cfgRedditUsername :: Text,
    -- | Reddit password for the bot (set via $REDDIT_PASSWORD)
    cfgRedditPassword :: Text,
    -- | Lock to prevent multiple threads writing to stdout at once.
    cfgLock :: MVar (),
    -- | Channel to pass posts to Discord.
    cfgChan :: Chan NotifyEvent,
    -- | Whether the app is running on Fly.io.
    cfgOnFly :: Bool
  }

-- | App configuration.
getConfig :: IO Config
getConfig = do
  cfgOnFly <- isJust <$> lookupEnv "FLY_APP_NAME"
  cfgDiscordToken <- T.pack <$> getEnv "DISCORD_APRIBOT_TOKEN"
  cfgRedditId <- T.pack <$> getEnv "REDDIT_ID"
  cfgRedditSecret <- T.pack <$> getEnv "REDDIT_SECRET"
  cfgRedditFrontendId <- T.pack <$> getEnv "REDDIT_FE_ID"
  cfgRedditFrontendSecret <- T.pack <$> getEnv "REDDIT_FE_SECRET"
  cfgRedditUsername <- T.pack <$> getEnv "REDDIT_USERNAME"
  cfgRedditPassword <- T.pack <$> getEnv "REDDIT_PASSWORD"
  cfgStaticDir <- getDataFileName "static"

  cfgPostsDbPath <- getDataFileName "data/posts.db"
  cfgTokensDbPath <- getDataFileName "data/tokens.db"
  cfgClassifierPath <- getDataFileName "python/predict.py"

  let cfgPtrChannelId = if cfgOnFly then 1120783589928345661 else 1132714928810238062
      cfgBbeChannelId = if cfgOnFly then 1120783566889037834 else 1132714951014875246
      cfgPort = 8080
      cfgUserAgent = "github:penelopeysm/apribot by /u/is_a_togekiss"
      cfgRedirectUri =
        if cfgOnFly
          then "https://apribot.fly.dev/authorised"
          else "http://localhost:8080/authorised"
  cfgLock <- newMVar ()
  cfgChan <- newChan
  pure $ Config {..}
