module Utils (atomically, getEnvAsText, markdownEscape, randomText, getSqlFileName) where

import Config
import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_apribot (getDataFileName)
import System.Environment (getEnv)
import System.Random

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

chars :: String
chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

randomText :: Int -> IO Text
randomText len = do
  T.pack . map (chars !!) . take len . randomRs (0, length chars - 1) <$> initStdGen

getSqlFileName :: (MonadIO m) => m String
getSqlFileName = liftIO $ getDataFileName (dbFileName config)
