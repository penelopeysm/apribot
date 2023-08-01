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
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Database (addNotifiedPost, checkNotifiedStatus)
import Database.SQLite.Simple (withConnection)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Pokeapi (PokeException (..))
import PokeapiBridge
import Reddit (Post (..), getPost, runRedditT)
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
        ha' <- atomically $ do
          print pkmn
          ha' <- liftIO $ try $ haSpecies (T.intercalate "-" . T.words $ pkmn)
          print ha'
          pure ha'
        case ha' of
          Left (_ :: PokeException) -> do
            randomApp <- liftIO randomAbility
            replyTo m Nothing $ "I don't think " <> pkmn <> " is a Pokemon, but if it was, it would have the hidden ability " <> randomApp <> "!"
          Right ps -> do
            let makeText (name, Nothing) = name <> " has no hidden ability"
                makeText (name, Just x) = name <> "'s hidden ability is: " <> x
            replyTo m Nothing $ T.intercalate "\n" (map makeText ps)
      -- Respond to spin-out thread requests
      -- TODO: factorise this out and maybe use an ExceptT. Goodness that indentation.
      when ("!thread" `T.isPrefixOf` T.toLower msgText) $ do
        case messageReference m of
          Nothing -> replyTo m Nothing "You should use `!thread` when replying to your trading partner's message"
          Just ref -> do
            case (referenceChannelId ref, referenceMessageId ref) of
              (Just rcid, Just rmid) -> do
                eitherRepliedMsg <- lift $ restCall $ DR.GetChannelMessage (rcid, rmid)
                case eitherRepliedMsg of
                  Left err -> do
                    atomically $ print err
                    replyTo m Nothing "Could not get info about the message you replied to."
                  Right repliedMsg -> do
                    let authorId1 = userId (messageAuthor m)
                        authorId2 = userId (messageAuthor repliedMsg)
                    author1 <- getUserNick (messageGuildId m) (messageAuthor m)
                    author2 <- getUserNick (messageGuildId m) (messageAuthor repliedMsg)
                    eitherFirstMsg <- lift $ restCall $ DR.GetChannelMessages rcid (1, DR.AfterMessage 0)
                    case eitherFirstMsg of
                      Left errr' -> do
                        atomically $ print errr'
                        replyTo m Nothing "Could not get info about the first message in the channel."
                      Right [firstMsg] -> do
                        atomically $ print (messageAuthor firstMsg)
                        when (authorId1 == userId (messageAuthor firstMsg)) $ do
                          when (authorId1 == authorId2) $ replyTo m Nothing "You can't trade with yourself!"
                          when (authorId1 /= authorId2) $ do
                            eitherChannel <- lift $ restCall $ DR.GetChannel rcid
                            case eitherChannel of
                              Left errr -> do
                                atomically $ print errr
                                replyTo m Nothing "Could not get info about the channel you replied to."
                              Right chn@(ChannelPublicThread {}) -> do
                                case channelThreadName chn of
                                  Nothing -> replyTo m Nothing "You can only use this command within a forum thread."
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
                                                      DR.startThreadRateLimit = Nothing
                                                    },
                                                DR.startThreadNoMessageType = 11, -- public thread
                                                DR.startThreadNoMessageInvitable = Nothing
                                              }
                                    case eitherNewThread of
                                      Left err' -> do
                                        atomically $ print err'
                                        replyTo m Nothing "Could not create thread."
                                      Right newThread -> do
                                        replyTo m (Just [authorId1, authorId2]) $ "Created thread for <@" <> tshow authorId1 <> "> and <@" <> tshow authorId2 <> "> at: " <> getChannelUrl newThread
                                        restCall_ $
                                          DR.CreateMessageDetailed (channelId newThread) $
                                            def
                                              { DR.messageDetailedContent = "Original post by <@" <> tshow authorId1 <> ">: https://discord.com/channels/" <> tshow (channelGuild chn) <> "/" <> tshow rcid <> "/" <> tshow (messageId firstMsg),
                                                DR.messageDetailedAllowedMentions = Just $ mentionOnly []
                                              }
                                        restCall_ $
                                          DR.CreateMessageDetailed (channelId newThread) $
                                            def
                                              { DR.messageDetailedContent = T.unlines . map ("> " <>) . T.lines $ messageContent firstMsg,
                                                DR.messageDetailedAllowedMentions = Just $ mentionOnly []
                                              }
                                        restCall_ $
                                          DR.CreateMessageDetailed (channelId newThread) $
                                            def
                                              { DR.messageDetailedContent = "Reply by <@" <> tshow authorId2 <> ">: https://discord.com/channels/" <> tshow (channelGuild chn) <> "/" <> tshow rcid <> "/" <> tshow rmid,
                                                DR.messageDetailedAllowedMentions = Just $ mentionOnly []
                                              }
                                        restCall_ $
                                          DR.CreateMessageDetailed (channelId newThread) $
                                            def
                                              { DR.messageDetailedContent = T.unlines . map ("> " <>) . T.lines $ messageContent repliedMsg,
                                                DR.messageDetailedAllowedMentions = Just $ mentionOnly []
                                              }
                              _ -> replyTo m Nothing "You can only use this command within a forum thread."
                      _ -> replyTo m Nothing "Could not get info about the first message in the channel."
              _ -> replyTo m Nothing "Could not get info about the message you replied to."
      -- Respond to thread closure
      when (any (`T.isPrefixOf` T.toLower msgText) ["!close", "[close]"]) $ do
        let channelId = messageChannelId m
        eitherChannel <- lift $ restCall $ DR.GetChannel channelId
        case eitherChannel of
          Left err -> do
            atomically $ print err
            replyTo m Nothing "Could not get info about the channel you replied in."
          Right chn@(ChannelPublicThread {}) -> do
            atomically $ print chn
            eitherFirstMsg <- lift $ restCall $ DR.GetChannelMessages channelId (1, DR.AfterMessage 0)
            case eitherFirstMsg of
              Right [firstMsg] -> do
                -- cfgDiscordId has the same value but wrong type for this
                -- comparison
                let threadCreatorId = userId (messageAuthor firstMsg)
                let closerId = userId (messageAuthor m)
                when
                  -- Case 1: ApriBot spun out a thread for a user
                  ( ( threadCreatorId == (DiscordId . unId) (cfgDiscordId cfg)
                        && ("Original post by <@" <> tshow closerId <> ">:") `T.isPrefixOf` messageContent firstMsg
                    )
                      || (threadCreatorId == closerId) -- Case 2: the user made the thread themselves
                  )
                  $ do
                    replyTo m Nothing "Closing and locking thread now; please ping a moderator if you need it reopened!"
                    restCall_ $
                      DR.ModifyChannel channelId $
                        def
                          { DR.modifyChannelName = Just $ "[Closed] " <> fromMaybe "" (channelThreadName chn),
                            DR.modifyChannelThreadArchived = Just True,
                            DR.modifyChannelThreadLocked = Just True
                          }
              _ -> replyTo m Nothing "Could not get the first message in the channel."
          Right _ ->
            pure ()
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
  -- Post the post into the appropriate channel, if it hasn't already been
  -- notified about
  let notifyPost :: Post -> App DiscordHandler ()
      notifyPost post = do
        postsSql <- asks cfgPostsDbPath
        alreadyNotified <- liftIO $ withConnection postsSql $ checkNotifiedStatus (postId post)
        case alreadyNotified of
          Just _ -> pure ()
          Nothing -> do
            let notify chanId = do
                  eitherMessage <- lift $ restCall $ DR.CreateMessageDetailed chanId (makeMessageDetails post)
                  pure $ case eitherMessage of
                    Right m -> Just m
                    Left _ -> Nothing
            maybeMessage <-
              case T.toLower (postSubreddit post) of
                "pokemontrades" -> notify (cfgPtrChannelId cfg)
                "bankballexchange" -> notify (cfgBbeChannelId cfg)
                _ -> pure Nothing
            case maybeMessage of
              Nothing -> pure ()
              Just msg -> do
                liftIO $ withConnection postsSql $ addNotifiedPost (postId post) (messageChannelId msg) (messageId msg)
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

replyTo :: Message -> Maybe [UserId] -> T.Text -> App DiscordHandler ()
replyTo m allowedMentions txt =
  restCall_ $
    DR.CreateMessageDetailed
      (messageChannelId m)
      ( def
          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
            DR.messageDetailedContent = txt,
            DR.messageDetailedAllowedMentions = mentionOnly <$> allowedMentions
          }
      )

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

getChannelUrl :: Channel -> T.Text
getChannelUrl c = "https://discord.com/channels/" <> tshow (channelGuild c) <> "/" <> tshow (channelId c)

mentionOnly :: [UserId] -> DR.AllowedMentions
mentionOnly userIds =
  DR.AllowedMentions
    { DR.mentionEveryone = False,
      DR.mentionUsers = False,
      DR.mentionRoles = False,
      DR.mentionUserIds = userIds,
      DR.mentionRoleIds = [],
      DR.mentionRepliedUser = True
    }

-- | Attempt to get user's server nickname. Falls back to global username.
getUserNick :: Maybe GuildId -> User -> App DiscordHandler T.Text
getUserNick mbGuildId user = do
  let fallback = userName user
  case mbGuildId of
    Nothing -> pure fallback
    Just guildId -> do
      eitherMember <- lift $ restCall $ DR.GetGuildMember guildId (userId user)
      case eitherMember of
        Left _ -> pure fallback
        Right member -> case memberNick member of
          Nothing -> pure fallback
          Just nick -> pure nick
