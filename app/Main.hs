module Main where

import Control.Concurrent.Async (mapConcurrently_)
import DiscordBot (discordBot)
import RedditBot (redditBot)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Trans
import Web (web)

-- | Entry point
main :: IO ()
main = runApp $ App $ do
  cfg <- ask
  if cfgOnFly cfg
    then liftIO $ putStrLn "Running on Fly.io"
    else liftIO $ putStrLn "Running locally"

  -- Not required when running locally, but for some reason, if I don't disable
  -- buffering, the output can't be seen on Fly.io logs.
  liftIO $ hSetBuffering stdout NoBuffering
  liftIO $
    mapConcurrently_
      (runAppWith cfg) 
      [web, redditBot, discordBot]
