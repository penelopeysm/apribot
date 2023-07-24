-- | Discord bot.
--
-- Note that we should actually like to implement a transformer stack like
--
--      DiscordT (App IO) ()
--
-- However, the discord-haskell library doesn't actually provide a DiscordT
-- transformer, so we do it the other way round, i.e.
--
--      App DiscordHandler ()
--
-- where DiscordHandler is a synonym for ReaderT DiscordHandle IO.
module DiscordBot (notifyDiscord, discordBot) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Pokeapi
import Reddit (Post (..), getPost, runRedditT)
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
            discordOnEvent = runAppWith cfg . eventHandler,
            discordOnStart = atomicallyWith lock $ putStrLn "Starting Discord bot..."
          }
  atomically $ T.putStrLn err

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: NotifyEvent -> App IO ()
notifyDiscord e = do
  chan <- asks cfgChan
  liftIO $ writeChan chan e

-- TODO: return a better type (lol)
getHA :: Text -> IO (Either SomeException (Maybe Text))
getHA p = try $ do
  pkmn <- pokemon p
  case filter paIsHidden (pokemonAbilities pkmn) of
    [] -> pure Nothing
    (x : _) -> do
      abty <- ability $ name (paAbility x)
      let englishName = filter (\n -> name (nameLanguage n) == "en") (abilityNames abty)
      pure $ case englishName of
        [] -> Nothing
        (n : _) -> Just $ nameName n

-- | Discord event handler. Right now, this does two things:
--
-- 1. Starts the channelLoop when the bot is initialised.
-- 2. Responds to messages from myself. This will be removed at some point in
--    time, but the idea is that we might want to respond to other events in the
--    future.
eventHandler :: Event -> App DiscordHandler ()
eventHandler e = do
  cfg <- ask
  let restCall_ = lift . void . restCall
  -- Print the event
  atomically $ print e >> putStrLn "" >> putStrLn ""
  case e of
    -- Begin notification loop
    Ready {} -> do
      hdl <- lift ask
      liftIO $ void $ forkIO $ (`runReaderT` hdl) $ runAppWith cfg notifyLoop
    -- Respond to HA requests
    MessageCreate m -> do
      let msg = messageContent m
      when ("!ha " `T.isPrefixOf` msg) $ do
        let pkmn = T.strip . T.drop 3 $ msg
        atomically $ print pkmn
        ha <- liftIO $ getHA pkmn
        atomically $ print ha
        restCall_ $
          DR.CreateMessageDetailed
            (messageChannelId m)
            ( def
                { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                  DR.messageDetailedContent = case ha of
                    Left _ -> "Could not find Pokemon named '" <> pkmn <> "'"
                    Right Nothing -> pkmn <> " has no hidden ability"
                    Right (Just x) -> pkmn <> "'s hidden ability is: " <> x
                }
            )
    -- Ignore other events (for now)
    _ -> pure ()

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
notifyLoop :: App DiscordHandler ()
notifyLoop = do
  cfg <- ask
  let startup = if cfgOnFly cfg then "Fly.io" else "localhost"
  let restCall_ = lift . void . restCall
  -- Notify that the bot has started, in my private channel.
  now <- systemSeconds <$> liftIO getSystemTime
  restCall_ $
    DR.CreateMessage
      1132000877415247903
      ("ApriBot started on " <> startup <> " at: <t:" <> T.pack (show now) <> ">")
  -- Post the post into the appropriate channel
  let notifyPost :: Post -> App DiscordHandler ()
      notifyPost post = do
        case T.toLower (postSubreddit post) of
          "pokemontrades" ->
            restCall_ $
              DR.CreateMessageDetailed
                (cfgPtrChannelId cfg)
                (makeMessageDetails post)
          "bankballexchange" ->
            restCall_ $
              DR.CreateMessageDetailed
                (cfgBbeChannelId cfg)
                (makeMessageDetails post)
          _ -> pure ()
  -- Loop
  let chan = cfgChan cfg
  forever $ do
    event <- liftIO $ readChan chan
    case event of
      NotifyPostById pid -> do
        redditEnv <- authenticateAsOwner
        post <- runRedditT redditEnv $ getPost pid
        notifyPost post
      NotifyPost post -> do
        notifyPost post

-- * Helper functions

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
