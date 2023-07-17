module Config (Config (..), config) where

import Discord.Types
import Data.Text (Text)

data Config = Config
  { dbFileName :: FilePath,
    tokenDbFileName :: FilePath,
    watchedSubreddit :: Text,
    discordChannelId :: ChannelId,
    port :: Int,
    keywords :: [Text],
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
      watchedSubreddit = "pokemontrades",
      discordChannelId = DiscordId $ Snowflake 1130599509689384963,
      port = thePort,
      keywords = ["apri", "dream", "beast", "safari", "sport", "fast", "friend", "heavy", "level", "love", "lure", "moon"],
      userAgent = "github:penelopeysm/apribot by /u/is_a_togekiss",
      redirectUri = "https://apribot.fly.dev/authorised",
      -- redirectUri = T.pack (printf "http://localhost:%d/authorised" thePort),
      pythonClassifier = "python/predict.py"
    }
