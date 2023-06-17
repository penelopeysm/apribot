module Utils (atomically, getEnvAsText, markdownEscape) where

import Control.Concurrent (MVar, withMVar)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnv)

atomically :: MVar () -> IO () -> IO ()
atomically lock action = withMVar lock $ const action

getEnvAsText :: Text -> IO Text
getEnvAsText = fmap T.pack . getEnv . T.unpack

-- | In principle, more characters should be escaped. However, the only texts
-- we're escaping (fornow) are Reddit usernames, and I think the underscore is
-- the only character that is both allowed in a username and has a special
-- meaning in Markdown.
markdownEscape :: Text -> Text
markdownEscape = T.replace "_" "\\_"
