{-# LANGUAGE LambdaCase #-}

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
import Control.Monad (forM, forM_)
import Data.Char.WCWidth (wcwidth)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Database (addNotifiedPost, checkNotifiedStatus)
import Database.SQLite.Simple (withConnection)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Natures (getRecommendedNatures)
import Pokeapi (PokeException (..))
import PokeapiBridge
import Reddit (ID (..), Post (..), getPost, runRedditT)
import Text.Printf (printf)
import Trans
import Utils (makeTable)

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
  -- Print if it's not a GuildCreate
  case e of
    GuildCreate {} -> pure ()
    _ -> atomically $ print e >> putStrLn "" >> putStrLn ""
  case e of
    GuildCreate {} -> pure ()
    MessageReactionAdd rinfo -> do
      cfg <- ask
      case cfgRoleReactionsMessageId cfg of
        Just mid -> do
          when (reactionMessageId rinfo == mid) $ do
            addRole (reactionGuildId rinfo) (reactionUserId rinfo) (reactionEmoji rinfo)
        Nothing -> pure ()
    MessageCreate m -> do
      cfg <- ask
      -- If running locally, ignore messages from other users
      if not (cfgOnFly cfg) && userId (messageAuthor m) /= 236863453443260419
        then pure ()
        else do
          let msgText = T.strip (messageContent m)
          when ("!boost" == T.toLower msgText) (replyTo m Nothing "I'm a bot, I can't boost a server.")
          when ("!help" == T.toLower msgText) (respondHelp m)
          when ("!potluck1" == T.toLower msgText) (respondPotluckVotes m)
          when ("!potluck2" == T.toLower msgText) (respondPotluckSignup m)
          when ("!em " `T.isPrefixOf` T.toLower msgText) (respondEM m)
          when ("!ha " `T.isPrefixOf` T.toLower msgText) (respondHA m)
          when ("!nature " `T.isPrefixOf` T.toLower msgText) (respondNature m)
          when ("!thread" `T.isPrefixOf` T.toLower msgText) (makeThread m)
          when (any (`T.isPrefixOf` T.toLower msgText) ["!close", "[close]"]) (closeThread m)
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
      notifyPost post = when (isNothing $ postDeleted post) $ do
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
  -- Delete the post (if it's not supposed to be there)
  let unnotifyPost :: ID Post -> App DiscordHandler ()
      unnotifyPost pid = do
        postsSql <- asks cfgPostsDbPath
        notified <- liftIO $ withConnection postsSql $ checkNotifiedStatus pid
        case notified of
          Nothing -> pure ()
          Just (chanId, msgId) -> restCall_ $ DR.DeleteMessage (chanId, msgId)
  -- Loop
  let chan = cfgChan cfg
  forever $ do
    event <- liftIO $ readChan chan
    case event of
      NotifyPostById pid -> do
        redditEnv <- authenticateAsOwner
        post <- runRedditT redditEnv $ getPost pid
        notifyPost post
      NotifyPost post -> notifyPost post
      UnnotifyPostById pid -> unnotifyPost pid

createEmEmbedDescription :: EggMove -> Text
createEmEmbedDescription em' =
  let parents = sortOn order (emParents em')
      levelUpParents =
        case mapMaybe
          ( \case
              LevelUpParent nm _ lvl -> Just $ speciesNameToRealName nm <> " (" <> tshow lvl <> ")"
              BothParent nm _ lvl -> Just $ speciesNameToRealName nm <> " (" <> tshow lvl <> ")"
              BreedParent {} -> Nothing
          )
          parents of
          [] -> []
          xs -> ["**Parents which learn by level up**\n" <> T.intercalate ", " xs]
      breedParents =
        case mapMaybe
          ( \case
              BreedParent nm _ -> Just $ speciesNameToRealName nm
              _ -> Nothing
          )
          parents of
          [] -> []
          xs -> ["**Parents which learn by breeding**\n" <> T.intercalate ", " xs <> " (and evolutions)"]
   in T.intercalate "\n\n" (emFlavorText em' : levelUpParents <> breedParents)

respondNature :: Message -> App DiscordHandler ()
respondNature m = do
  let msgText = T.strip (messageContent m)
      pkmn = T.strip . T.drop 7 $ msgText
      natureText = getRecommendedNatures pkmn
  replyTo m Nothing natureText

respondEM :: Message -> App DiscordHandler ()
respondEM m = do
  channel <- lift $ restCall $ DR.GetChannel (messageChannelId m)
  let isDm = case channel of
        Right (ChannelDirectMessage {}) -> True
        _ -> False
  let msgWords = T.words . T.toLower . T.strip $ messageContent m
  result <- case msgWords of
    "!em" : "usum" : rest -> do
      let pkmn = T.intercalate "-" rest
      ems <- liftIO $ try $ em USUM pkmn
      pure $ Right (pkmn, "USUM" :: Text, ems)
    "!em" : "swsh" : rest -> do
      let pkmn = T.intercalate "-" rest
      ems <- liftIO $ try $ em SwSh pkmn
      pure $ Right (pkmn, "SwSh" :: Text, ems)
    "!em" : "bdsp" : rest -> do
      let pkmn = T.intercalate "-" rest
      ems <- liftIO $ try $ em BDSP pkmn
      pure $ Right (pkmn, "BDSP", ems)
    "!em" : "sv" : rest -> do
      let pkmn = T.intercalate "-" rest
      ems <- liftIO $ try $ em SV pkmn
      pure $ Right (pkmn, "SV", ems)
    _ -> pure $ Left "usage: `!em game pokemon` (game is usum, bdsp, swsh, or sv). e.g. `!em swsh togepi`"
  case result of
    Left err -> replyTo m Nothing err
    Right (pkmn, game, ems) -> do
      case ems of
        -- Not a Pokemon
        Left (_ :: PokeException) -> do
          moveDescs <- liftIO randomMoves
          let messageText = "I don't think " <> pkmn <> " is a Pokemon, but if it was, it would have the egg moves: " <> T.intercalate ", " (map fst moveDescs) <> "!"
          let embed =
                def
                  { createEmbedTitle = "Descriptions",
                    createEmbedDescription = T.intercalate "\n" (map (\(m', d') -> "**" <> m' <> "**: " <> d') moveDescs),
                    createEmbedColor = Just DiscordColorLuminousVividPink
                  }
          restCall_ $
            DR.CreateMessageDetailed
              (messageChannelId m)
              ( def
                  { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                    DR.messageDetailedContent = messageText,
                    DR.messageDetailedAllowedMentions = Nothing,
                    DR.messageDetailedEmbeds = Just [embed]
                  }
              )
        -- No egg moves
        Right [] ->
          replyTo m Nothing . T.pack $ printf "%s has no egg moves in %s" (speciesNameToRealName pkmn) game
        -- Egg moves
        Right ems' -> do
          let emText = T.pack $ printf "%s egg moves in %s: %s" (speciesNameToRealName pkmn) game (T.intercalate ", " (map emName ems'))
          let messageText =
                if isDm
                  then emText
                  else emText <> "\n\nFor full details about compatible parents, use this command in a DM with me!"
          let makeEmEmbedWithParents :: EggMove -> CreateEmbed
              makeEmEmbedWithParents em' =
                def
                  { createEmbedTitle = emName em',
                    createEmbedDescription = createEmEmbedDescription em',
                    createEmbedColor = Just DiscordColorLuminousVividPink
                  }
          let emEmbedShort =
                def
                  { createEmbedTitle = "Descriptions",
                    createEmbedDescription = T.intercalate "\n" (map (\e -> "**" <> emName e <> "**: " <> emFlavorText e) ems'),
                    createEmbedColor = Just DiscordColorLuminousVividPink
                  }
          restCall_ $
            DR.CreateMessageDetailed
              (messageChannelId m)
              ( def
                  { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                    DR.messageDetailedContent = messageText,
                    DR.messageDetailedAllowedMentions = Nothing,
                    DR.messageDetailedEmbeds =
                      if isDm
                        then Just (take 9 $ map makeEmEmbedWithParents ems')
                        else Just [emEmbedShort]
                  }
              )
          -- Send a second message with the rest of the egg moves. We assume no
          -- species has more than 16 egg moves.
          when (length ems' > 9 && isDm) $
            restCall_ $
              DR.CreateMessageDetailed
                (messageChannelId m)
                ( def
                    { DR.messageDetailedReference = Nothing,
                      DR.messageDetailedContent = "(continued from above)",
                      DR.messageDetailedAllowedMentions = Nothing,
                      DR.messageDetailedEmbeds =
                        if isDm
                          then Just (drop 9 $ map makeEmEmbedWithParents ems')
                          else Nothing
                    }
                )

respondHA :: Message -> App DiscordHandler ()
respondHA m = do
  let msgText = T.strip (messageContent m)
      pkmn = T.strip . T.drop 3 $ msgText
      makeEmbed :: (Text, Text) -> Maybe CreateEmbed
      makeEmbed (ha', desc') =
        if desc' == ""
          then Nothing
          else
            Just $
              def
                { createEmbedTitle = ha',
                  createEmbedDescription = desc',
                  createEmbedUrl =
                    "https://bulbapedia.bulbagarden.net/wiki/"
                      <> (T.intercalate "_" . T.words $ ha')
                      <> "_(Ability)",
                  createEmbedColor = Just DiscordColorLuminousVividPink
                }
  haDetails <- atomically $ liftIO $ try $ ha (T.intercalate "-" . T.words $ pkmn)
  case haDetails of
    -- Not a Pokemon
    Left (_ :: PokeException) -> do
      (ha', desc') <- liftIO randomAbility
      restCall_ $
        DR.CreateMessageDetailed
          (messageChannelId m)
          ( def
              { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                DR.messageDetailedContent = "I don't think " <> pkmn <> " is a Pokemon, but if it was, it would have the hidden ability " <> ha' <> "!",
                DR.messageDetailedAllowedMentions = Nothing,
                DR.messageDetailedEmbeds = (: []) <$> makeEmbed (ha', desc')
              }
          )
    -- One species that has a HA
    Right [(name, Just (ha', desc'))] ->
      restCall_ $
        DR.CreateMessageDetailed
          (messageChannelId m)
          ( def
              { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                DR.messageDetailedContent = speciesNameToRealName name <> "'s hidden ability is: " <> ha',
                DR.messageDetailedAllowedMentions = Nothing,
                DR.messageDetailedEmbeds = (: []) <$> makeEmbed (ha', desc')
              }
          )
    -- One species with no HA
    Right [(name, Nothing)] -> replyTo m Nothing $ name <> " has no hidden ability"
    -- Multiple species
    Right species -> do
      let makeText (name, Nothing) = speciesNameToRealName name <> " has no hidden ability"
          makeText (name, Just (ha', _)) = speciesNameToRealName name <> "'s hidden ability is: " <> ha'
          messageText = T.intercalate "\n" (map makeText species)
          uniqueHAs = nub $ mapMaybe snd species
          embeds = mapMaybe makeEmbed uniqueHAs
      restCall_ $
        DR.CreateMessageDetailed
          (messageChannelId m)
          ( def
              { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                DR.messageDetailedContent = messageText,
                DR.messageDetailedAllowedMentions = Nothing,
                DR.messageDetailedEmbeds = Just embeds
              }
          )

respondPotluckVotes :: Message -> App DiscordHandler ()
respondPotluckVotes m = do
  let canonicaliseEmoji e = case emojiName e of
        "beastball" -> Just "Bea"
        "dreamball" -> Just "Dre"
        "fastball" -> Just "Fas"
        "friendball" -> Just "Fri"
        "heavyball" -> Just "Hea"
        "levelball" -> Just "Lev"
        "loveball" -> Just "Lov"
        "lureball" -> Just "Lur"
        "moonball" -> Just "Moo"
        "safariball" -> Just "Saf"
        "sportball" -> Just "Spo"
        _ -> Nothing
  -- eyes react because this one takes a while
  restCall_ $ DR.CreateReaction (messageChannelId m, messageId m) ":eyes:"
  -- Fetch latest message from the #potluck-signup channel
  cid <- asks cfgPotluckVotesChannelId
  guildId <- asks cfgAprimarketGuildId
  eitherMsgs <- lift $ restCall $ DR.GetChannelMessages cid (100, DR.AfterMessage 0)
  let extractValidReactionsWithCount :: MessageReaction -> Maybe (Text, Int)
      extractValidReactionsWithCount r = do
        emoji <- canonicaliseEmoji . messageReactionEmoji $ r
        pure (emoji, messageReactionCount r)
  case eitherMsgs of
    Left _ -> replyTo m Nothing "Could not fetch messages from #potluck-vote"
    Right msgs -> do
      let nonBotMsgs = filter (not . userIsBot . messageAuthor) msgs
          emojiOrder = ["Bea", "Dre", "Fas", "Fri", "Hea", "Lev", "Lov", "Lur", "Moo", "Saf", "Spo"]
      voteRows <- forM nonBotMsgs $ \msg ->
        do
          let text = messageContent msg
          authorServerNick <- getUserNickFromMessage msg (Just guildId)
          let msgReactions = M.fromList $ mapMaybe extractValidReactionsWithCount (messageReactions msg)
              totalVotes = sum $ M.elems msgReactions
              ballVotes = map (\e -> (" " <>) . T.pack . show . fromMaybe 0 . M.lookup e $ msgReactions) emojiOrder
          pure (totalVotes, text : authorServerNick : T.pack (show totalVotes) : ballVotes)
      let sortedVoteRows = map snd $ sortOn (Down . fst) voteRows
          headerRow = "Pokemon" : "Proposed by" : "Total" : emojiOrder
          table = makeTable (headerRow : sortedVoteRows) True (Just 5)
      replyWithFile
        m
        ("Current status of proposals in " <> "<#" <> tshow cid <> ">:")
        "potluck-votes.txt"
        table

respondPotluckSignup :: Message -> App DiscordHandler ()
respondPotluckSignup m = do
  let canonicaliseEmoji e = case emojiName e of
        "\128175" -> Just ("100" :: Text)
        "beastball" -> Just "Bea"
        "dreamball" -> Just "Dre"
        "fastball" -> Just "Fas"
        "friendball" -> Just "Fri"
        "heavyball" -> Just "Hea"
        "levelball" -> Just "Lev"
        "loveball" -> Just "Lov"
        "lureball" -> Just "Lur"
        "moonball" -> Just "Moo"
        "safariball" -> Just "Saf"
        "sportball" -> Just "Spo"
        _ -> Nothing
  -- eyes react because this one takes a while
  restCall_ $ DR.CreateReaction (messageChannelId m, messageId m) ":eyes:"
  -- Fetch latest message from the #potluck-signup channel
  plSignupChannelId <- asks cfgPotluckSignupChannelId
  guildId <- asks cfgAprimarketGuildId
  eitherMsg <- lift $ restCall $ DR.GetChannelMessages plSignupChannelId (1, DR.LatestMessages)
  case eitherMsg of
    Right [latestMsg] -> do
      -- Grab reactions
      let emojis = map messageReactionEmoji (messageReactions latestMsg)
      maybeReactions <- forM emojis $ \e -> do
        case canonicaliseEmoji e of
          Just simpleName -> do
            let emoji = case emojiId e of
                  Nothing -> emojiName e -- Builtin emoji
                  Just eid -> emojiName e <> ":" <> T.pack (show eid) -- Custom emoji
            users <- lift $ restCall $ DR.GetReactions (plSignupChannelId, messageId latestMsg) emoji (100, DR.LatestReaction)
            case users of
              Right users' -> pure $ Just (simpleName, users')
              Left _ -> pure Nothing
          Nothing -> pure Nothing
      -- Construct ASCII table
      let reactions = M.fromList . catMaybes $ maybeReactions
          allUsers = nub . concat . M.elems $ reactions
      userNicknames <- fmap M.fromList $ forM allUsers $ \u -> do
        let uid = userId u
        nick <- T.strip . T.filter (\c -> wcwidth c == 1) <$> getUserNick u (Just guildId)
        pure (uid, nick)
      let headerRow = "User" : M.keys reactions
          userRow user = (userNicknames M.! userId user) : map (\k -> if user `elem` (reactions M.! k) then " x " else "") (M.keys reactions)
          userRows = map userRow allUsers
          table = makeTable (headerRow : userRows) True (Just 5)

      replyWithFile
        m
        ("Reactions to latest message in " <> "<#" <> tshow plSignupChannelId <> ">:")
        "potluck-signups.txt"
        table
    _ -> replyTo m Nothing "Could not get latest message from #potluck-signup"

-- | Temporary function for algorithm testing
--
-- NOT CURRENTLY USED
_respondPotluckSignup2 :: Message -> App DiscordHandler ()
_respondPotluckSignup2 m = do
  let canonicaliseEmoji e = case emojiName e of
        "\128175" -> Just ("100" :: Text)
        "beastball" -> Just "Bea"
        "dreamball" -> Just "Dre"
        "fastball" -> Just "Fas"
        "friendball" -> Just "Fri"
        "heavyball" -> Just "Hea"
        "levelball" -> Just "Lev"
        "loveball" -> Just "Lov"
        "lureball" -> Just "Lur"
        "moonball" -> Just "Moo"
        "safariball" -> Just "Saf"
        "sportball" -> Just "Spo"
        _ -> Nothing
  -- Fetch latest message from the #potluck-signup channel
  plChannelId <- asks cfgPotluckSignupChannelId
  guildId <- asks cfgAprimarketGuildId
  eitherMsg <- lift $ restCall $ DR.GetChannelMessages plChannelId (1, DR.LatestMessages)
  case eitherMsg of
    Right [latestMsg] -> do
      -- Grab reactions
      let emojis = map messageReactionEmoji (messageReactions latestMsg)
      maybeReactions <- forM emojis $ \e -> do
        case canonicaliseEmoji e of
          Just simpleName -> do
            let emoji = case emojiId e of
                  Nothing -> emojiName e -- Builtin emoji
                  Just eid -> emojiName e <> ":" <> T.pack (show eid) -- Custom emoji
            users <- lift $ restCall $ DR.GetReactions (plChannelId, messageId latestMsg) emoji (100, DR.LatestReaction)
            case users of
              Right users' -> pure $ Just (simpleName, users')
              Left _ -> pure Nothing
          Nothing -> pure Nothing
      -- Construct ASCII table but IGNORING 100s for now
      let reactions = M.filterWithKey (\k _ -> k /= "100") $ M.fromList . catMaybes $ maybeReactions
          allUsers = nub . concat . M.elems $ reactions
      -- Construct row vector of total reacts
      let nReactions = T.intercalate "," . map (T.pack . show . length) $ M.elems reactions
      -- Construct matrix of reacts
      let userRow user = T.intercalate "," $ map (\k -> if user `elem` (reactions M.! k) then "1" else "0") (M.keys reactions)
          userRows = map userRow allUsers
          table = T.unlines userRows
      -- Print column names
      let colNames = T.intercalate "," (M.keys reactions)
      -- Print row names
      userNicknames <- fmap M.fromList $ forM allUsers $ \u -> do
        let uid = userId u
        nick <- T.strip . T.filter (\c -> wcwidth c == 1) <$> getUserNick u (Just guildId)
        let quotedNick = if T.any (== ',') nick then "\"" <> nick <> "\"" else nick
        pure (uid, quotedNick)
      let rowNames = T.intercalate "," $ map (\user -> userNicknames M.! userId user) allUsers

      replyTo m Nothing $
        T.unlines
          [ "Total reactions to latest message in #potluck-signup",
            "```",
            nReactions,
            "```",
            "",
            "Reaction matrix",
            "```",
            table,
            "```",
            "",
            "Column names",
            "```",
            colNames,
            "```",
            "",
            "Row names",
            "```",
            rowNames,
            "```"
          ]
    _ -> replyTo m Nothing "Could not get latest message from #potluck-signup"

makeThread :: Message -> App DiscordHandler ()
makeThread m = do
  cfg <- ask
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
              author1 <- getUserNickFromMessage m (messageGuildId m)
              author2 <- getUserNickFromMessage repliedMsg (messageGuildId m)
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
                              let threadName = truncateThreadTitle $ author1 <> " & " <> author2 <> " | " <> tname
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
                                                DR.startThreadAutoArchive = Just 10080,
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

closeThread :: Message -> App DiscordHandler ()
closeThread m = do
  cfg <- ask
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
                    { DR.modifyChannelName = Just $ truncateThreadTitle $ "[Closed] " <> fromMaybe "" (channelThreadName chn),
                      DR.modifyChannelThreadArchived = Just True,
                      DR.modifyChannelThreadLocked = Just True
                    }
        _ -> replyTo m Nothing "Could not get the first message in the channel."
    Right _ ->
      pure ()

respondHelp :: Message -> App DiscordHandler ()
respondHelp m = do
  votesChan <- asks cfgPotluckVotesChannelId
  signupChan <- asks cfgPotluckSignupChannelId
  replyTo m Nothing $
    T.unlines
      [ "**General commands**",
        "- `!help`",
        "  Show this message.",
        "- `!ha {pokemon}`",
        "  Show the hidden ability of a Pokémon",
        "- `!em {game} {pokemon}`",
        "  Show egg moves for a Pokémon in a game. `{game}` can be `usum`, `bdsp`, `swsh`, or `sv`. If you use this command in a DM with the bot, it will also list potential parents.",
        "- `!nature {pokemon}`",
        "  Show suggested natures for a Pokémon (collated from a couple of spreadsheets).",
        "- `!potluck1`",
        "  Show a summary of the proposals in <#" <> tshow votesChan <> ">.",
        "- `!potluck2`",
        "  Show a summary of reactions to the most recent post in <#" <> tshow signupChan <> ">.",
        "**Trading commands**",
        "- `!thread`",
        "  Reply to your trading partner in the trading forums with this to create a new thread in #thread-archive",
        "- `!close`",
        "  Close your trading post, or a thread you created"
      ]

ballRoles :: Map Text RoleId
ballRoles =
  M.fromList
    [ ("beastball", 1142586472751444108),
      ("dreamball", 1142587632111595603),
      ("fastball", 1142587859212193793),
      ("friendball", 1142588024518103110),
      ("heavyball", 1142588347236237384),
      ("levelball", 1142589194523377674),
      ("loveball", 1142589700725547112),
      ("lureball", 1142589833395589230),
      ("moonball", 1142589959879000136),
      ("safariball", 1142590142515785840),
      ("sportball", 1142590314364817508)
    ]

addRole :: Maybe GuildId -> UserId -> Emoji -> App DiscordHandler ()
addRole maybeGuildId uid emoji =
  case maybeGuildId of
    Nothing -> pure ()
    Just gid -> do
      case M.lookup (emojiName emoji) ballRoles of
        Just rid -> do
          forM_ (M.elems ballRoles) $ \rid' -> do
            restCall_ $ DR.RemoveGuildMemberRole gid uid rid'
          restCall_ $ DR.AddGuildMemberRole gid uid rid
        Nothing -> pure ()

-- * Helper functions

-- | For convenience
restCall_ :: (Request (r a), FromJSON a) => r a -> App (ReaderT DiscordHandle IO) ()
restCall_ req = do
  resp <- lift $ restCall req
  case resp of
    Left e -> atomically $ print e
    Right _ -> pure ()

cleanRedditMarkdown :: Text -> Text
cleanRedditMarkdown = T.replace "#" "\\#" . T.replace "&#x200B;" "" . T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">"

-- | Get the first element of a list, if it exists
mbHead :: Maybe [a] -> Maybe a
mbHead Nothing = Nothing
mbHead (Just []) = Nothing
mbHead (Just (x : _)) = Just x

summarisePostBody :: Post -> Text
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
      embedColor = case (flair, T.toLower (postSubreddit post)) of
        (Just "SV", "pokemontrades") -> DiscordColorRGB 122 53 55
        (Just "SV", "bankballexchange") -> DiscordColorRGB 216 46 50
        (Just "SWSH", "pokemontrades") -> DiscordColorRGB 133 77 192
        (Just "SWSH", "bankballexchange") -> DiscordColorRGB 76 159 231
        (Just "BDSP", "pokemontrades") -> DiscordColorRGB 184 142 226
        (Just "BDSP", "bankballexchange") -> DiscordColorRGB 96 208 187
        (Just "SMUSUM", "pokemontrades") -> DiscordColorRGB 230 150 63
        (Just "SMUSUM", "bankballexchange") -> DiscordColorRGB 239 140 58
        (Just "Giveaway", "pokemontrades") -> DiscordColorRGB 58 135 123
        (Just "GIVEAWAY", "bankballexchange") -> DiscordColorRGB 237 239 241
        (Just "CROSS-GEN", "bankballexchange") -> DiscordColorRGB 248 214 88
        (Just "Tradeback", "pokemontrades") -> DiscordColorRGB 128 170 72
        (Just "Home", "pokemontrades") -> DiscordColorRGB 70 155 80
        (Just "XYORAS", "pokemontrades") -> DiscordColorRGB 38 65 149
        (Just "Shiny", "pokemontrades") -> DiscordColorRGB 189 91 135
        (Just "LGPE", "pokemontrades") -> DiscordColorRGB 207 178 59
        (Just "Contest", "pokemontrades") -> DiscordColorRGB 48 113 114
        (Just "Event", "pokemontrades") -> DiscordColorRGB 237 113 170
        (Just s, "pokemontrades") ->
          if "(Closed)" `T.isSuffixOf` s
            then DiscordColorRGB 189 189 189
            else DiscordColorRGB 235 145 163 -- A nice shade of pink
        _ -> DiscordColorRGB 235 145 163 -- A nice shade of pink
   in def
        { DR.messageDetailedEmbeds =
            Just
              [ def
                  { createEmbedUrl = postUrl post,
                    createEmbedTitle = truncatedTitle,
                    createEmbedDescription = summarisePostBody post,
                    createEmbedFooterText = footerText,
                    createEmbedColor = Just embedColor,
                    createEmbedTimestamp = Just (postCreatedTime post)
                  }
              ],
          DR.messageDetailedContent = ""
        }

replyTo :: Message -> Maybe [UserId] -> Text -> App DiscordHandler ()
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

tshow :: (Show a) => a -> Text
tshow = T.pack . show

getChannelUrl :: Channel -> Text
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
getUserNickFromMessage :: Message -> Maybe GuildId -> App DiscordHandler Text
getUserNickFromMessage m maybeGid =
  let auth = messageAuthor m
   in case maybeGid of
        Nothing -> pure $ userName auth
        Just gid -> do
          let fallback = case userGlobalName auth of
                Nothing -> userName auth
                Just gn -> gn
          maybeGuildMember <- case messageMember m of
            Just mem -> pure (Just mem)
            Nothing -> do
              eitherGuildMember <- lift $ restCall $ DR.GetGuildMember gid (userId auth)
              case eitherGuildMember of
                Left _ -> pure Nothing
                Right mem -> pure (Just mem)
          pure $ fromMaybe fallback (maybeGuildMember >>= memberNick)

-- | Attempt to get user's server nickname, but from a User instead of a
-- message. Falls back to global username.
--
-- TODO: refactor to avoid code duplication with the above
getUserNick :: User -> Maybe GuildId -> App DiscordHandler Text
getUserNick u maybeGid =
  case maybeGid of
    Nothing -> pure $ userName u
    Just gid -> do
      let fallback = case userGlobalName u of
            Nothing -> userName u
            Just gn -> gn
      maybeGuildMember <- do
        eitherGuildMember <- lift $ restCall $ DR.GetGuildMember gid (userId u)
        case eitherGuildMember of
          Left _ -> pure Nothing
          Right mem -> pure (Just mem)
      pure $ fromMaybe fallback (maybeGuildMember >>= memberNick)

-- | Truncate a thread title to 100 characters
truncateThreadTitle :: Text -> Text
truncateThreadTitle t = if T.length t > 100 then T.take 97 t <> "..." else t

replyWithFile :: Message -> Text -> Text -> Text -> App DiscordHandler ()
replyWithFile m msgContents fname fcontents = do
  restCall_ $
    DR.CreateMessageDetailed
      (messageChannelId m)
      ( def
          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
            DR.messageDetailedContent = msgContents,
            DR.messageDetailedAllowedMentions = Just (mentionOnly []),
            DR.messageDetailedFile = Just (fname, TE.encodeUtf8 fcontents)
          }
      )
