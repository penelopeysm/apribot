module Main where

import Control.Concurrent (newMVar)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.Chan (newChan)
import DiscordBot (discordBot)
import RedditBot (redditBot)
import Web (web)

-- | Entry point
main :: IO ()
main = do
  stdoutLock <- newMVar ()
  discordChan <- newChan
  mapConcurrently_
    id
    [web stdoutLock, redditBot stdoutLock discordChan, discordBot stdoutLock discordChan]
