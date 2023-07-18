module DiscordBot (notifyDiscord, discordBot) where

import Config
import Control.Concurrent (MVar, forkIO, putMVar, takeMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Reddit (Post (..))
import System.Environment (getEnv)
import Utils

-- | Run the Discord bot.
discordBot :: MVar () -> MVar Post -> IO ()
discordBot stdoutLock discordLock = do
  discordToken <- T.pack <$> getEnv "DISCORD_APRIBOT_TOKEN"
  err <-
    runDiscord $
      def
        { discordToken = discordToken,
          discordOnEvent = eventHandler discordLock (discordChannelId config),
          discordOnStart = liftIO $ atomically stdoutLock $ putStrLn "Starting Discord bot..."
        }
  atomically stdoutLock $ T.putStrLn err

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: MVar Post -> Post -> IO ()
notifyDiscord discordLock post = do
  putMVar discordLock post

-- | Discord event handler. Right now, this does two things:
--
-- 1. Starts the channelLoop when the bot is initialised.
-- 2. Responds to messages from myself. This will be removed at some point in
--    time, but the idea is that we might want to respond to other events in the
--    future.
eventHandler :: MVar Post -> ChannelId -> Event -> DiscordHandler ()
eventHandler discordLock cid e = do
  case e of
    Ready {} -> do
      env <- ask
      liftIO $ void $ forkIO $ runReaderT (channelLoop discordLock cid) env
    MessageCreate m -> do
      let myUserId = DiscordId $ Snowflake 236863453443260419
      when (userId (messageAuthor m) == myUserId) $ do
        void $
          restCall $
            DR.CreateMessageDetailed
              (messageChannelId m)
              ( def
                  { DR.messageDetailedAllowedMentions = Just (def {DR.mentionUserIds = [myUserId]}),
                    DR.messageDetailedContent = "hello <@236863453443260419> :heart:"
                  }
              )
    _ -> pure ()

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
channelLoop :: MVar Post -> ChannelId -> DiscordHandler ()
channelLoop discordLock cid = do
  post <- liftIO $ takeMVar discordLock
  void $ restCall $ DR.CreateMessage cid (postUrl post)
  channelLoop discordLock cid
