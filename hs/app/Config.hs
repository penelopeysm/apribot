module Config (Config (..), getConfig, NotifyEvent (..)) where

import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Database.PostgreSQL.Simple as PSQL
import Discord.Types
import Paths_apribot (getDataFileName)
import Reddit (ID, Post)
import System.Environment (getEnv, lookupEnv)

mkId :: Word64 -> DiscordId a
mkId = DiscordId . Snowflake

data NotifyEvent
  = NotifyPost Post
  | NotifyPostById (ID Post)
  | UnnotifyPostById (ID Post)
  | Log Text
  | Debug Text
  deriving (Show)

data Config = Config
  { -- | Path to web app static directory
    cfgStaticDir :: FilePath,
    -- | Guild id
    cfgAprimarketGuildId :: GuildId,
    -- | Discord channel to post /r/pokemontrades posts to.
    cfgPtrChannelId :: ChannelId,
    -- | Discord channel to post /r/bankballexchange posts to.
    cfgBbeChannelId :: ChannelId,
    -- | Discord channel to create trade overflow threads in
    cfgTradeOverflowChannelId :: ChannelId,
    -- | Message to monitor for role reactions
    cfgRoleReactionsMessageId :: Maybe MessageId,
    -- | Discord channel for #potluck-vote
    cfgPotluckVotesChannelId :: ChannelId,
    -- | Discord channel for #potluck-signup
    cfgPotluckSignupChannelId :: ChannelId,
    -- | Discord channel to log to
    cfgLogChannelId :: ChannelId,
    -- | Discord channel to debug to
    cfgDebugChannelId :: ChannelId,
    -- | Port to listen on.
    cfgPort :: Int,
    -- | User agent to use for Reddit API requests.
    cfgUserAgent :: Text,
    -- | Redirect URI for OAuth2.
    cfgRedirectUri :: Text,
    -- | Path to the Python classifier script.
    cfgClassifierPath :: FilePath,
    -- | Path to the Python LLM script.
    cfgLLMPath :: FilePath,
    -- | My own Discord user ID
    cfgPennyId :: UserId,
    -- | The bot's own Discord user ID
    cfgApribotId :: UserId,
    -- | Discord token (set via $DISCORD_TOKEN)
    cfgDiscordToken :: Text,
    -- | Discord application ID (set via $DISCORD_ID)
    cfgDiscordId :: ApplicationId,
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
    -- | Seconds between checking for Reddit posts
    cfgRedditStreamDelay :: Double,
    -- | Lock to prevent multiple threads writing to stdout at once.
    cfgLock :: MVar (),
    -- | Channel to pass posts to Discord.
    cfgChan :: Chan NotifyEvent,
    -- | Whether the app is running on Fly.io.
    cfgOnFly :: Bool,
    -- | Function to retrieve PostgreSQL connection string
    cfgPsqlConn :: IO PSQL.Connection,
    -- | Current execution context, for logging
    cfgContext :: Text
  }

-- | App configuration.
getConfig :: IO Config
getConfig = do
  cfgOnFly <- isJust <$> lookupEnv "FLY_APP_NAME"
  cfgDiscordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  cfgDiscordId <- read <$> getEnv "DISCORD_ID"
  cfgRedditId <- T.pack <$> getEnv "REDDIT_ID"
  cfgRedditSecret <- T.pack <$> getEnv "REDDIT_SECRET"
  cfgRedditFrontendId <- T.pack <$> getEnv "REDDIT_FE_ID"
  cfgRedditFrontendSecret <- T.pack <$> getEnv "REDDIT_FE_SECRET"
  cfgRedditUsername <- T.pack <$> getEnv "REDDIT_USERNAME"
  cfgRedditPassword <- T.pack <$> getEnv "REDDIT_PASSWORD"
  cfgStaticDir <- getDataFileName "static"

  let cfgPennyId = mkId 236863453443260419
  let cfgApribotId = mkId 1130570595176812546

  cfgClassifierPath <- getDataFileName "python/predict.py"
  cfgLLMPath <- getDataFileName "python/langtest.py"

  let cfgAprimarketGuildId = mkId 1120782351811739689
      cfgPtrChannelId = mkId $ if cfgOnFly then 1120783589928345661 else 1132714928810238062
      cfgBbeChannelId = mkId $ if cfgOnFly then 1120783566889037834 else 1132714951014875246
      cfgTradeOverflowChannelId = mkId $ if cfgOnFly then 1133716162308489216 else 1133555612538646629
      cfgPotluckVotesChannelId = mkId 1144082505704681555
      cfgPotluckSignupChannelId = mkId 1144082624613204029
      cfgLogChannelId = mkId 1132000877415247903
      cfgDebugChannelId = mkId 1179134931461480519
      -- Disabled for now
      cfgRoleReactionsMessageId = Nothing
      cfgPort = 8080
      cfgUserAgent = "github:penelopeysm/apribot by /u/is_a_togekiss"
      cfgRedirectUri =
        if cfgOnFly
          then "https://apribot.fly.dev/authorised"
          else "http://localhost:5173/authorised"
      cfgRedditStreamDelay = 15
  cfgLock <- newMVar ()
  cfgChan <- newChan

  let cfgContext = "apribot"

  let cfgPsqlConn =
        if cfgOnFly
          then -- Production
          do
            maybeUrl <- lookupEnv "DATABASE_URL"
            case maybeUrl of
              Nothing -> error "Running on Fly.io, but DATABASE_URL not set"
              Just url -> PSQL.connectPostgreSQL $ TE.encodeUtf8 $ T.pack url
          else -- Local database
          do
            maybeUrl <- lookupEnv "FLY_PG_PROXY_CONN_STRING"
            case maybeUrl of
              Nothing -> error "Running locally, but FLY_PG_PROXY_CONN_STRING not set. Use `fly proxy 5432 -a apripsql`"
              Just url -> PSQL.connectPostgreSQL $ TE.encodeUtf8 $ T.pack url

  pure $ Config {..}
