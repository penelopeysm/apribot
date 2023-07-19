{-# LANGUAGE DeriveGeneric #-}

module DiscordBot (notifyDiscord, discordBot) where

import Config
import Control.Concurrent (MVar, forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import GHC.Generics (Generic)
import Reddit (Post (..))
import System.Environment (getEnv)
import System.Random
import System.Random.Stateful (globalStdGen, uniformM)
import Utils

-- | Others don't seem to work
data HeartEmoji
  = Heart
  | BlueHeart
  | GreenHeart
  | PurpleHeart
  | YellowHeart
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Uniform HeartEmoji

instance Show HeartEmoji where
  show Heart = "heart"
  show BlueHeart = "blue_heart"
  show GreenHeart = "green_heart"
  show PurpleHeart = "purple_heart"
  show YellowHeart = "yellow_heart"

-- | Run the Discord bot.
discordBot :: MVar () -> Chan Post -> IO ()
discordBot stdoutLock discordChan = do
  discordToken <- T.pack <$> getEnv "DISCORD_APRIBOT_TOKEN"
  err <-
    runDiscord $
      def
        { discordToken = discordToken,
          discordOnEvent = eventHandler discordChan,
          discordOnStart = liftIO $ atomically stdoutLock $ putStrLn "Starting Discord bot..."
        }
  atomically stdoutLock $ T.putStrLn err

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: Chan Post -> Post -> IO ()
notifyDiscord discordChan post = do
  writeChan discordChan post

-- | Discord event handler. Right now, this does two things:
--
-- 1. Starts the channelLoop when the bot is initialised.
-- 2. Responds to messages from myself. This will be removed at some point in
--    time, but the idea is that we might want to respond to other events in the
--    future.
eventHandler :: Chan Post -> Event -> DiscordHandler ()
eventHandler discordChan e = do
  case e of
    Ready {} -> do
      env <- ask
      liftIO $ void $ forkIO $ runReaderT (notifyLoop discordChan) env
    MessageCreate m -> do
      let myUserId = DiscordId $ Snowflake 236863453443260419
      when (userId (messageAuthor m) == myUserId) $ do
        randomHeart :: HeartEmoji <- uniformM globalStdGen
        void . restCall $
          DR.CreateReaction (messageChannelId m, messageId m) (T.pack $ show randomHeart)
    _ -> pure ()

cleanPostBody :: T.Text -> T.Text
cleanPostBody = T.replace "&#x200B;" "" . T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">"

summarisePostBody :: Post -> T.Text
summarisePostBody post =
  let body = cleanPostBody (postBody post)
      maxWords = 30
      maxChars = 4096 -- Discord API limit
      ws = T.words body
      line = T.unwords ws
   in if length ws <= maxWords && T.length line <= maxChars
        then line
        else
          if length ws <= maxWords && T.length line > maxChars
            then T.take (maxChars - 3) line <> "..."
            else (T.take (maxChars - 3) . T.unwords . take maxWords $ ws) <> "..."

makeMessageDetails :: Post -> DR.MessageDetailedOpts
makeMessageDetails post =
  let maxTitleLength = 256 -- Discord API limit
      embedTitle = if T.length (postTitle post) > maxTitleLength then T.take (maxTitleLength - 3) (postTitle post) <> "..." else postTitle post
   in def
        { DR.messageDetailedEmbeds =
            Just
              [ def
                  { createEmbedUrl = postUrl post,
                    createEmbedTitle = embedTitle,
                    createEmbedDescription = summarisePostBody post,
                    createEmbedAuthorName = "/u/" <> postAuthor post,
                    createEmbedAuthorUrl = "https://reddit.com/u/" <> postAuthor post,
                    createEmbedColor = Just DiscordColorLuminousVividPink,
                    createEmbedTimestamp = Just (postCreatedTime post)
                  }
              ],
          DR.messageDetailedContent = ""
        }

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
notifyLoop :: Chan Post -> DiscordHandler ()
notifyLoop discordChan = do
  -- Notify that the bot has started, in my private channel.
  now <- liftIO getCurrentTime
  void . restCall $
    DR.CreateMessageDetailed
      (DiscordId $ Snowflake 1130599509689384963)
      ( def
          { DR.messageDetailedEmbeds =
              Just
                [ def
                    { createEmbedTitle = "ApriBot startup notification",
                      createEmbedDescription = "Hello, I'm awake! :heart:",
                      createEmbedColor = Just DiscordColorLuminousVividPink,
                      createEmbedTimestamp = Just now
                    }
                ],
            DR.messageDetailedContent = ""
          }
      )
  forever $ do
    post <- liftIO $ readChan discordChan
    case T.toLower (postSubreddit post) of
      "pokemontrades" ->
        void . restCall $
          DR.CreateMessageDetailed
            (ptradesChannelId config)
            (makeMessageDetails post)
      "bankballexchange" ->
        void . restCall $
          DR.CreateMessageDetailed
            (bbeChannelId config)
            (makeMessageDetails post)
      _ -> pure ()
