module Main where

import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Concurrent.Chan (Chan, newChan)
import DiscordBot (discordBot)
import Reddit (Post)
import RedditBot (redditBot)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Web (web)

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  stdoutLock :: MVar () <- newMVar ()
  discordChan :: Chan Post <- newChan
  -- Run Reddit bot, Discord bot, and web server concurrently
  _ <- forkIO $ web stdoutLock
  _ <- forkIO $ redditBot stdoutLock discordChan
  discordBot stdoutLock discordChan
