module Main where

import Control.Concurrent.Async (mapConcurrently_)
import DiscordBot (discordBot)
import Options.Applicative
import RedditBot (redditBot)
import RoleCheck (roleCheck)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Trans
import Web (web)

-- | Command-line options
data Options = Options
  { optPerformRoleCheckOnly :: Bool,
    optRunBackendOnly :: Bool
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      ( long "role-check"
          <> short 'r'
          <> help "Perform role check and immediately exit"
      )
    <*> switch
      ( long "backend-only"
          <> short 'b'
          <> help "Only run web backend and not Reddit/Discord bots"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc "Run the bot"
        <> header "ApriBot :: https://apribot.fly.dev"
    )

-- | Entry point
main :: IO ()
main = do
  options <- execParser optionsInfo
  case (optPerformRoleCheckOnly options, optRunBackendOnly options) of
    (True, True) -> error "cannot specify -r and -b simultaneously"
    (True, False) -> runApp roleCheck
    (False, True) -> runApp web
    (False, False) -> runApp $ App $ do
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
