module Config (Config (..), config) where

import Data.Text (Text)
import Reddit

data Config = Config
  { dbFileName :: String,
    watchedSubreddit :: Text,
    notifyOnPostId :: ID Post,
    port :: Int,
    keywords :: [Text],
    userAgent :: Text
  }

-- | App configuration.
config :: Config
config =
  Config
    { dbFileName = "/data/posts.db",
      watchedSubreddit = "pokemontrades",
      notifyOnPostId = PostID "13fzjhf",
      port = 8080,
      keywords = ["apri", "dream", "beast", "safari", "sport", "fast", "friend", "heavy", "level", "love", "lure", "moon"],
      userAgent = "github:penelopeysm/apribot by /u/is_a_togekiss"
    }
