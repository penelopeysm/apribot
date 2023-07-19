module Main where

import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Concurrent.Chan (Chan, newChan)
import DiscordBot
import Reddit (Post)
import RedditBot (redditBot)
import System.IO
import Web

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- Run Reddit bot, Discord bot, and web server concurrently
  stdoutLock :: MVar () <- newMVar ()
  discordChan :: Chan Post <- newChan
  _ <- forkIO $ web stdoutLock
  _ <- forkIO $ redditBot stdoutLock discordChan
  discordBot stdoutLock discordChan
