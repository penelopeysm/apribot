module Main where

import Control.Concurrent (newMVar)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.Chan (newChan)
import DiscordBot (discordBot)
import RedditBot (redditBot)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Web (web)

-- | Entry point
main :: IO ()
main = do
  -- Not required when running locally, but for some reason, if I don't disable
  -- buffering, the output can't be seen on Fly.io logs.
  hSetBuffering stdout NoBuffering
  stdoutLock <- newMVar ()
  discordChan <- newChan
  mapConcurrently_
    id
    [web stdoutLock, redditBot stdoutLock discordChan, discordBot stdoutLock discordChan]
