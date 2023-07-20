module Config (Config (..), config) where

import Discord.Types
import Data.Text (Text)

data Config = Config
  { dbFileName :: FilePath,
    tokenDbFileName :: FilePath,
    ptradesChannelId :: ChannelId,
    bbeChannelId :: ChannelId,
    port :: Int,
    userAgent :: Text,
    redirectUri :: Text,
    pythonClassifier :: FilePath
  }

thePort :: Int
thePort = 8080

-- | App configuration.
config :: Config
config =
  Config
    { dbFileName = "/data/posts.db",
      tokenDbFileName = "/data/tokens.db",
      ptradesChannelId = DiscordId $ Snowflake 1120783589928345661,
      bbeChannelId = DiscordId $ Snowflake 1120783566889037834,
      port = thePort,
      userAgent = "github:penelopeysm/apribot by /u/is_a_togekiss",
      redirectUri = "https://apribot.fly.dev/authorised",
      -- redirectUri = T.pack (printf "http://localhost:%d/authorised" thePort),
      pythonClassifier = "python/predict.py"
    }
