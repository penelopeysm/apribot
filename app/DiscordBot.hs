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
import Control.Exception (try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Pokeapi
import Reddit (Post (..), getPost, runRedditT)
import System.Random (randomRIO)
import Trans

-- | Run the Discord bot.
discordBot :: App IO ()
discordBot = do
  discordToken <- asks cfgDiscordToken
  cfg <- ask
  err <-
    liftIO $
      runDiscord $
        def
          { discordToken = discordToken,
            discordOnEvent = runAppWith cfg . eventHandler,
            discordOnStart = runAppWith cfg startup
          }
  atomically $ T.putStrLn err

-- | Actions to run on startup.
startup :: App DiscordHandler ()
startup = do
  cfg <- ask
  hdl <- lift ask
  -- Start notification loop (awkward types because unlift and lift)
  liftIO $ void $ forkIO $ (`runReaderT` hdl) $ runAppWith cfg notifyLoop
  -- Notify that the bot has started, in my private channel.
  let platform = if cfgOnFly cfg then "Fly.io" else "localhost"
  now <- systemSeconds <$> liftIO getSystemTime
  restCall_ $
    DR.CreateMessage
      1132000877415247903
      ("ApriBot started on " <> platform <> " at: <t:" <> T.pack (show now) <> ">")

-- | Discord event handler. Right now, this does two things:
--
-- 1. Starts the channelLoop when the bot is initialised.
-- 2. Responds to messages from myself. This will be removed at some point in
--    time, but the idea is that we might want to respond to other events in the
--    future.
eventHandler :: Event -> App DiscordHandler ()
eventHandler e = do
  cfg <- ask
  -- Print the event
  atomically $ print e >> putStrLn "" >> putStrLn ""
  case e of
    -- Respond to HA requests
    MessageCreate m -> do
      let msgText = T.strip (messageContent m)
      when ("!ha " `T.isPrefixOf` T.toLower msgText) $ do
        let pkmn = T.strip . T.drop 3 $ msgText
        atomically $ print pkmn
        ha <- liftIO $ try $ getHiddenAbility pkmn
        randomApp <- liftIO randomAbility
        atomically $ print ha
        replyTo m $ case ha of
          Left (_ :: PokeException) -> "I don't think " <> pkmn <> " is a Pokemon, but if it was, it would have the hidden ability " <> randomApp <> "!"
          Right Nothing -> pkmn <> " has no hidden ability"
          Right (Just x) -> pkmn <> "'s hidden ability is: " <> x
      -- Respond to spin-out thread requests
      -- TODO: factorise this out and maybe use an ExceptT. Goodness that indentation.
      when ("!thread" == T.toLower msgText) $ do
        case messageReference m of
          Nothing -> replyTo m "You should use `!thread` when replying to your trading partner's message"
          Just ref -> do
            case (referenceChannelId ref, referenceMessageId ref) of
              (Just rcid, Just rmid) -> do
                eitherRepliedMsg <- lift $ restCall $ DR.GetChannelMessage (rcid, rmid)
                case eitherRepliedMsg of
                  Left _ -> replyTo m "Could not get info about the message you replied to."
                  Right repliedMsg -> do
                    let authorId1 = userId $ messageAuthor m
                        author1 = userName $ messageAuthor m
                        authorId2 = userId $ messageAuthor repliedMsg
                        author2 = userName $ messageAuthor repliedMsg
                    -- TODO: Get the first message in the channel and check that
                    -- the person using !thread is the OP.
                    when (authorId1 == authorId2) $ replyTo m "You can't trade with yourself!"
                    when (authorId1 /= authorId2) $ do
                      eitherChannel <- lift $ restCall $ DR.GetChannel rcid
                      case eitherChannel of
                        Right chn@(ChannelPublicThread {}) -> do
                          case channelThreadName chn of
                            Nothing -> replyTo m "You can only use this command within a forum thread."
                            Just tname -> do
                              let threadName = author1 <> " & " <> author2 <> " | " <> tname
                              atomically $ T.putStrLn $ "Creating thread for " <> author1 <> " and " <> author2
                              eitherNewThread <-
                                lift $
                                  restCall $
                                    DR.StartThreadNoMessage
                                      (cfgTradeOverflowChannelId cfg)
                                      DR.StartThreadNoMessageOpts
                                        { DR.startThreadNoMessageBaseOpts =
                                            DR.StartThreadOpts
                                              { DR.startThreadName = threadName,
                                                DR.startThreadAutoArchive = Just 1440,
                                                DR.startThreadRateLimit = Just 3
                                              },
                                          DR.startThreadNoMessageType = 11, -- public thread
                                          DR.startThreadNoMessageInvitable = Nothing
                                        }
                              case eitherNewThread of
                                Left _ -> replyTo m "Could not create thread."
                                Right newThread -> do
                                  replyTo m $ "Created thread for " <> author1 <> " and " <> author2 <> " at: " <> getChannelUrl newThread
                                  -- TODO: If we get the first message in the
                                  -- thread, we should add in the message ID at
                                  -- the end of the following line.
                                  restCall_ $ DR.CreateMessage (channelId newThread) $ "Original post by " <> author1 <> ": https://discord.com/channels/" <> tshow (channelGuild chn) <> "/" <> tshow rcid
                                  restCall_ $ DR.CreateMessage (channelId newThread) $ "Reply by " <> author2 <> ": https://discord.com/channels/" <> tshow (channelGuild chn) <> "/" <> tshow rcid <> "/" <> tshow rmid
                        _ -> replyTo m "You can only use this command within a forum thread."
              _ -> replyTo m "Could not get info about the message you replied to."
    -- Ignore other events (for now)
    _ -> pure ()

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: NotifyEvent -> App IO ()
notifyDiscord e = do
  chan <- asks cfgChan
  liftIO $ writeChan chan e

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
notifyLoop :: App DiscordHandler ()
notifyLoop = do
  cfg <- ask
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

-- | Generate a random ability. TODO: Should probably be refactored into Pokeapi
randomAbility :: IO Text
randomAbility = do
  abId :: Int <- randomRIO (0, 358)
  abty <- ability (T.pack $ show abId)
  let englishName = filter (\n -> name (nameLanguage n) == "en") (abilityNames abty)
  pure $ case englishName of
    [] -> "You managed to break the bot. Congratulations!"
    (n : _) -> nameName n

-- | For convenience
restCall_ :: (Request (r a), FromJSON a) => r a -> App (ReaderT DiscordHandle IO) ()
restCall_ req = do
  resp <- lift $ restCall req
  case resp of
    Left e -> atomically $ print e
    Right _ -> pure ()

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

replyTo :: Message -> T.Text -> App DiscordHandler ()
replyTo m txt =
  restCall_ $
    DR.CreateMessageDetailed
      (messageChannelId m)
      ( def
          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
            DR.messageDetailedContent = txt
          }
      )

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

getChannelUrl :: Channel -> T.Text
getChannelUrl c = "https://discord.com/channels/" <> tshow (channelGuild c) <> "/" <> tshow (channelId c)
