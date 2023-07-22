module DiscordBot (notifyDiscord, discordBot) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Reddit (Post (..))
import Trans

-- | Run the Discord bot.
discordBot :: App IO ()
discordBot = do
  discordToken <- asks cfgDiscordToken
  cfg <- ask
  lock <- asks cfgLock

  err <-
    liftIO $
      runDiscord $
        def
          { discordToken = discordToken,
            discordOnEvent = eventHandler cfg,
            discordOnStart = atomicallyWith lock $ putStrLn "Starting Discord bot..."
          }
  atomically $ T.putStrLn err

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
eventHandler :: Config -> Event -> DiscordHandler ()
eventHandler cfg e = do
  let lock = cfgLock cfg
  -- Print the event
  atomicallyWith lock $ print e >> putStrLn "" >> putStrLn ""
  case e of
    -- Begin notification loop
    Ready {} -> do
      env <- ask
      liftIO $ void $ forkIO $ runReaderT (notifyLoop cfg) env
    -- Ignore other events (for now)
    _ -> pure ()

cleanRedditMarkdown :: T.Text -> T.Text
cleanRedditMarkdown = T.replace "#" "\\#" . T.replace "&#x200B;" "" . T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">"

-- | Get the first element of a list, if it exists
mbHead :: Maybe [a] -> Maybe a
mbHead Nothing = Nothing
mbHead (Just []) = Nothing
mbHead (Just (x : _)) = Just x

summarisePostBody :: Post -> T.Text
summarisePostBody post =
  let body = case mbHead (postCrosspostParents post) of
        Nothing -> cleanRedditMarkdown (postBody post)
        Just xpost -> cleanRedditMarkdown (postBody xpost)
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
  let (footerText, flair) = case mbHead (postCrosspostParents post) of
        Nothing -> ("by /u/" <> postAuthor post, postFlairText post)
        Just xpost ->
          ( "xposted from /r/" <> postSubreddit xpost <> "\nby /u/" <> postAuthor xpost,
            postFlairText xpost
          )
      maxTitleLength = 256 -- Discord API limit
      title = case flair of
        Nothing -> cleanRedditMarkdown (postTitle post)
        Just f -> " [" <> f <> "] " <> cleanRedditMarkdown (postTitle post)
      truncatedTitle = if T.length title > maxTitleLength then T.take (maxTitleLength - 3) title <> "..." else title
   in def
        { DR.messageDetailedEmbeds =
            Just
              [ def
                  { createEmbedUrl = postUrl post,
                    createEmbedTitle = truncatedTitle,
                    createEmbedDescription = summarisePostBody post,
                    createEmbedFooterText = footerText,
                    createEmbedColor = Just DiscordColorLuminousVividPink,
                    createEmbedTimestamp = Just (postCreatedTime post)
                  }
              ],
          DR.messageDetailedContent = ""
        }

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
notifyLoop :: Config -> DiscordHandler ()
notifyLoop cfg = do
  -- Notify that the bot has started, in my private channel.
  now <- systemSeconds <$> liftIO getSystemTime
  void . restCall $
    DR.CreateMessage 1132000877415247903 ("ApriBot started at: <t:" <> T.pack (show now) <> ">")

  let chan = cfgChan cfg
  forever $ do
    post <- liftIO $ readChan chan
    case T.toLower (postSubreddit post) of
      "pokemontrades" ->
        void . restCall $
          DR.CreateMessageDetailed
            (cfgPtrChannelId cfg)
            (makeMessageDetails post)
      "bankballexchange" ->
        void . restCall $
          DR.CreateMessageDetailed
            (cfgBbeChannelId cfg)
            (makeMessageDetails post)
      _ -> pure ()
