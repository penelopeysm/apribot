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
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
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
import Data.Word (Word64)
import Database (addNotifiedPost, checkNotifiedStatus)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Pokemon
import Reddit (ID (..), Post (..), getPost, runRedditT)
import Text.Printf (printf)
import Trans
import Utils (makeTable)

mkId :: Word64 -> DiscordId a
mkId = DiscordId . Snowflake

mkFullName :: Text -> Maybe Text -> Text
mkFullName name Nothing = name
mkFullName name (Just form) = name <> " (" <> form <> ")"

withContext :: (Monad m) => Text -> App m a -> App m a
withContext newCtx = local $ \ctx ->
  let existing = cfgContext ctx
   in ctx {cfgContext = existing <> ":" <> newCtx}

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
startup = withContext "startup" $ do
  cfg <- ask
  hdl <- lift ask
  -- Start notification loop (awkward types because unlift and lift)
  liftIO $ void $ forkIO $ (`runReaderT` hdl) $ runAppWith cfg notifyLoop
  -- Notify that the bot has started, in my private channel.
  let platform = if cfgOnFly cfg then "Fly.io" else "localhost"
  now <- systemSeconds <$> liftIO getSystemTime
  restCall_ $
    DR.CreateMessage
      (cfgLogChannelId cfg)
      ("ApriBot started on " <> platform <> " at: <t:" <> tshow now <> ">")

-- | Discord event handler. Right now, this does two things:
--
-- 1. Starts the channelLoop when the bot is initialised.
-- 2. Responds to messages from myself. This will be removed at some point in
--    time, but the idea is that we might want to respond to other events in the
--    future.
eventHandler :: Event -> App DiscordHandler ()
eventHandler e = withContext "eventHandler" $ do
  -- Print if it's not a GuildCreate
  case e of
    GuildCreate {} -> pure ()
    _ -> atomically $ print e
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
      if not (cfgOnFly cfg) && userId (messageAuthor m) /= mkId 236863453443260419
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
          when ("!legality " `T.isPrefixOf` T.toLower msgText) (respondLegality m)
          when ("!thread" `T.isPrefixOf` T.toLower msgText) (makeThread m)
          when (any (`T.isPrefixOf` T.toLower msgText) ["!close", "[close]"]) (closeThread m)
    -- Ignore other events (for now)
    _ -> pure ()

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: (MonadIO m) => NotifyEvent -> App m ()
notifyDiscord e = do
  -- If e is a Log event, prefix it with the current execution context
  e' <- case e of
    Log msg -> do
      ctx <- asks cfgContext
      pure $ Log $ ctx <> ": " <> msg
    _ -> pure e
  chan <- asks cfgChan
  liftIO $ writeChan chan e'

-- | Loop which waits for a post to be added to the MVar. When one is added (via
-- the 'notifyDiscord' function), this posts it to the Discord channel.
notifyLoop :: App DiscordHandler ()
notifyLoop = withContext "notifyLoop" $ do
  cfg <- ask
  -- Post the post into the appropriate channel, if it hasn't already been
  -- notified about
  let notifyPost :: Post -> App DiscordHandler ()
      notifyPost post = when (isNothing $ postDeleted post) $ do
        alreadyNotified <- checkNotifiedStatus (postId post)
        when (isNothing alreadyNotified) $ do
          -- Determine which channel to post to
          let maybeChannelId = case T.toLower (postSubreddit post) of
                "pokemontrades" -> case postFlairText post of
                  Just flair ->
                    if "Closed" `T.isInfixOf` flair
                      then Nothing
                      else Just (cfgPtrChannelId cfg)
                  Nothing -> Nothing
                "bankballexchange" -> Just (cfgBbeChannelId cfg)
                _ -> Nothing
          case maybeChannelId of
            Nothing -> pure ()
            Just cid -> do
              -- Post the message
              eitherMessage <- lift $ restCall $ DR.CreateMessageDetailed cid (makeMessageDetails post)
              case eitherMessage of
                Left err -> do
                  withContext "notifyPost" $ tellError err
                Right msg -> do
                  -- If it's a ptrades post, add it to the notified posts in the DB
                  when (T.toLower (postSubreddit post) == "pokemontrades") $
                    addNotifiedPost
                      (postId post)
                      (messageChannelId msg)
                      (messageId msg)
  -- Delete the post (if it's not supposed to be there)
  let unnotifyPost :: ID Post -> App DiscordHandler ()
      unnotifyPost pid = do
        notified <- checkNotifiedStatus pid
        chanId <- asks cfgPtrChannelId
        case notified of
          Nothing -> pure ()
          Just msgId -> restCall_ $ DR.DeleteMessage (chanId, msgId)
  -- Post a log event to #apribot-logs in my server. The Text value that it's
  -- called with must contain all relevant info, none of it is added by this
  -- function
  let logToDiscord :: Text -> App DiscordHandler ()
      logToDiscord = restCall_ . DR.CreateMessage (cfgLogChannelId cfg)
  -- Loop
  let chan = cfgChan cfg
  forever $ do
    event <- liftIO $ readChan chan
    liftIO $ print event
    case event of
      NotifyPostById pid -> do
        redditEnv <- authenticateAsOwner
        post <- runRedditT redditEnv $ getPost pid
        notifyPost post
      NotifyPost post -> notifyPost post
      UnnotifyPostById pid -> unnotifyPost pid
      Log t -> logToDiscord t

createEmEmbedDescription :: EggMoveWithParents -> Text
createEmEmbedDescription em' =
  let levelUpParents =
        case mapMaybe
          ( \case
              LevelUpParent nm lvl -> Just $ nm <> " (" <> tshow lvl <> ")"
              EvolutionParent nm -> Just $ nm <> " (evolution)"
              BreedParent {} -> Nothing
          )
          (emwpParents em') of
          [] -> []
          xs -> ["**Parents which learn by level up**\n" <> T.intercalate ", " xs]
      breedParents =
        case mapMaybe
          ( \case
              BreedParent nm -> Just nm
              _ -> Nothing
          )
          (emwpParents em') of
          [] -> []
          xs -> ["**Parents which learn by breeding**\n" <> T.intercalate ", " xs]
   in T.intercalate "\n\n" (emwpFlavorText em' : levelUpParents <> breedParents)

respondNature :: Message -> App DiscordHandler ()
respondNature m = withContext ("respondNature (`" <> messageContent m <> "`)") $ do
  let msgText = T.strip (messageContent m)
      pkmn = T.strip . T.drop 7 $ msgText
  -- Try to fetch the Pokemon first.
  pkmnDetails <- getPokemonIdsAndDetails pkmn
  case pkmnDetails of
    [] -> replyTo m Nothing $ "No Pokémon with name '" <> pkmn <> "' found."
    [(pkmnId, pkmnName, pkmnForm, _)] -> do
      let fullName = mkFullName pkmnName pkmnForm
      suggestedNatures <- getSuggestedNatures pkmnId
      liftIO $ T.putStrLn "hi2"
      case suggestedNatures of
        Nothing -> replyTo m Nothing $ "No suggested natures found for " <> fullName <> "."
        Just sn -> do
          let text =
                "Suggested natures for "
                  <> fullName
                  <> ":"
                  <> case penny sn of
                    Nothing -> ""
                    Just n -> "\nPenny's sheet (mostly Pikalytics): " <> n
                  <> case jemmaSwSh sn of
                    Nothing -> ""
                    Just n -> "\nJemma's SwSh sheet (Smogon): " <> n
                  <> case jemmaBDSP sn of
                    Nothing -> ""
                    Just n -> "\nJemma's BDSP sheet (Smogon): " <> n
                  <> case jemmaG7 sn of
                    Nothing -> ""
                    Just n -> "\nJemma's G7 sheet (Smogon): " <> n
                  <> "\nOriginal sheets: https://tinyurl.com/tgkss"
          replyTo m Nothing text
    _ -> replyTo m Nothing "Found multiple matches: this should not happen, please let Penny know"

respondEM :: Message -> App DiscordHandler ()
respondEM m = withContext ("respondEM (`" <> messageContent m <> "`)") $ do
  let msgWords = T.words . T.toLower . T.strip $ messageContent m
  -- Parse message contents first
  result <- case msgWords of
    "!em" : "usum" : rest -> do
      let pkmn = T.intercalate "-" rest
      pure $ Right (pkmn, USUM)
    "!em" : "swsh" : rest -> do
      let pkmn = T.intercalate "-" rest
      pure $ Right (pkmn, SwSh)
    "!em" : "bdsp" : rest -> do
      let pkmn = T.intercalate "-" rest
      pure $ Right (pkmn, BDSP)
    "!em" : "sv" : rest -> do
      let pkmn = T.intercalate "-" rest
      pure $ Right (pkmn, SV)
    _ -> pure $ Left ()
  case result of
    -- If message contents don't make sense, reply with usage
    Left _ -> replyTo m Nothing "usage: `!em game pokemon` (game is usum, bdsp, swsh, or sv). e.g. `!em swsh togepi`"
    Right (pkmn, game) -> do
      -- Try to fetch the Pokemon first. If it can't be found, choose some random moves
      pkmnDetails <- getPokemonIdsAndDetails pkmn
      case pkmnDetails of
        [] -> do
          moveDescs <- randomMoves
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
        -- One exact match found. Calculate egg moves
        [(id', name, form, _)] -> do
          let fullName = mkFullName name form
          -- Check if the message is in a DM
          channel <- lift $ restCall $ DR.GetChannel (messageChannelId m)
          let isDm = case channel of
                Right (ChannelDirectMessage {}) -> True
                _ -> False
          -- If it is, respond in the DM
          if isDm
            then do
              ems <- getEmsWithParents game id'
              case ems of
                -- No egg moves
                [] ->
                  replyTo m Nothing . T.pack $ printf "%s has no egg moves in %s" fullName (show game)
                -- Egg moves
                ems' -> do
                  let makeEmEmbedWithParents :: EggMoveWithParents -> CreateEmbed
                      makeEmEmbedWithParents em' =
                        def
                          { createEmbedTitle = emwpName em',
                            createEmbedDescription = createEmEmbedDescription em',
                            createEmbedColor = Just DiscordColorLuminousVividPink
                          }
                  let emText = T.pack $ printf "%s egg moves in %s: %s" fullName (show game) (T.intercalate ", " (map emwpName ems'))
                  let embeds = map makeEmEmbedWithParents ems'
                  let postSubsequentEms remainingEmbeds =
                        case splitAt 5 remainingEmbeds of
                          ([], []) -> pure ()
                          (xs, ys) -> do
                            restCall_ $
                              DR.CreateMessageDetailed
                                (messageChannelId m)
                                ( def
                                    { DR.messageDetailedReference = Nothing,
                                      DR.messageDetailedContent = "",
                                      DR.messageDetailedAllowedMentions = Nothing,
                                      DR.messageDetailedEmbeds = Just xs
                                    }
                                )
                            postSubsequentEms ys
                  case splitAt 5 embeds of
                    (xs, ys) -> do
                      restCall_ $
                        DR.CreateMessageDetailed
                          (messageChannelId m)
                          ( def
                              { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                                DR.messageDetailedContent = emText,
                                DR.messageDetailedAllowedMentions = Nothing,
                                DR.messageDetailedEmbeds = Just xs
                              }
                          )
                      postSubsequentEms ys
            else do
              -- Not in DMs
              ems <- getEmsNoParents game id'
              case ems of
                -- No egg moves
                [] ->
                  replyTo m Nothing . T.pack $ printf "%s has no egg moves in %s" fullName (show game)
                -- Egg moves
                ems' -> do
                  let emText =
                        T.pack $
                          printf
                            "%s egg moves in %s: %s\nFor full details about parents, use this command in a DM."
                            fullName
                            (show game)
                            (T.intercalate ", " (map emnpName ems'))
                  let emEmbedShort =
                        def
                          { createEmbedTitle = "Descriptions",
                            createEmbedDescription = T.intercalate "\n" (map (\e -> "**" <> emnpName e <> "**: " <> emnpFlavorText e) ems'),
                            createEmbedColor = Just DiscordColorLuminousVividPink
                          }
                  restCall_ $
                    DR.CreateMessageDetailed
                      (messageChannelId m)
                      ( def
                          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                            DR.messageDetailedContent = emText,
                            DR.messageDetailedAllowedMentions = Nothing,
                            DR.messageDetailedEmbeds = Just [emEmbedShort]
                          }
                      )
        _ -> replyTo m Nothing "Found multiple matches: this should not happen, please let Penny know"

respondHA :: Message -> App DiscordHandler ()
respondHA m = withContext ("respondHA (`" <> messageContent m <> "`)") $ do
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
  -- Try to fetch the Pokemon first. If it can't be found, choose some random moves
  pkmnDetails <- getPokemonIdsAndDetails pkmn
  case pkmnDetails of
    -- Not a Pokemon
    [] -> do
      (ha', desc') <- randomAbility
      atomically $ print ha'
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
    -- One species
    [(pkmnId, name, form, _)] -> do
      let fullName = mkFullName name form
      let piplupDisclaimer =
            if name `elem` ["Piplup", "Prinplup", "Empoleon"]
              then "\n(Note that prior to SV, the Piplup family had Defiant as their HA.)"
              else ""
      haAndFlavorText <- ha pkmnId
      case haAndFlavorText of
        -- No HA
        Nothing -> replyTo m Nothing $ fullName <> " has no hidden ability."
        -- HA
        Just (ha', desc') -> do
          restCall_ $
            DR.CreateMessageDetailed
              (messageChannelId m)
              ( def
                  { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                    DR.messageDetailedContent = fullName <> "'s hidden ability is: " <> ha' <> piplupDisclaimer,
                    DR.messageDetailedAllowedMentions = Nothing,
                    DR.messageDetailedEmbeds = (: []) <$> makeEmbed (ha', desc')
                  }
              )
    _ -> replyTo m Nothing "Found multiple matches: this should not happen, please let Penny know"

respondLegality :: Message -> App DiscordHandler ()
respondLegality m = withContext ("respondLegality (`" <> messageContent m <> "`)") $ do
  let msgText = T.strip (messageContent m)
      pkmn = T.strip . T.drop 9 $ msgText
  -- Try to fetch the Pokemon first.
  pkmnDetails <- getPokemonIdsAndDetails pkmn
  case pkmnDetails of
    [] -> replyTo m Nothing $ "No Pokémon with name '" <> pkmn <> "' found."
    [(pkmnId, pkmnName, pkmnForm, _)] -> do
      unbreedable <- isPokemonUnbreedable pkmnId
      let showLegality :: GenLegality -> Text
          showLegality (GenLegality b d a s sp) =
            T.concat
              [ if b then "<:beastball:1132050100017959033>" else "",
                if d then "<:dreamball:1132050106200375416>" else "",
                if a then "<:fastball:1132050109073465414><:friendball:1132050111598436414><:heavyball:1132050112965775541><:levelball:1132050114765148260><:loveball:1132050117323661382><:lureball:1132050118481285220><:moonball:1132050120251281530>" else "",
                if s then "<:safariball:1132052412501344266>" else "",
                if sp then "<:sportball:1132050124823068752>" else ""
              ]
      let fullName = mkFullName pkmnName pkmnForm
      legalities <- getLegality pkmnId
      let message :: Text
          message =
            fullName
              <> " legality:\n"
              <> T.intercalate
                "\n"
                ( map
                    ( \(game, (availableInGame, legality)) ->
                        ( if availableInGame
                            then ":white_check_mark:"
                            else ":x:"
                        )
                          <> " **"
                          <> tshow game
                          <> "** "
                          <> case (availableInGame, unbreedable) of
                            (False, _) -> "Not available in game"
                            (True, True) -> "Cannot be bred"
                            (True, False) ->
                              if legality == GenLegality False False False False False
                                then "No ball combos available"
                                else showLegality legality
                    )
                    (M.assocs legalities)
                )
      replyTo m Nothing message
    _ -> replyTo m Nothing "Found multiple matches: this should not happen, please let Penny know"

respondPotluckVotes :: Message -> App DiscordHandler ()
respondPotluckVotes m = withContext "respondPotluckVotes" $ do
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
  eitherMsgs <- lift $ restCall $ DR.GetChannelMessages cid (100, DR.AfterMessage (mkId 0))
  let extractValidReactionsWithCount :: MessageReaction -> Maybe (Text, Int)
      extractValidReactionsWithCount r = do
        emoji <- canonicaliseEmoji . messageReactionEmoji $ r
        pure (emoji, messageReactionCount r)
  case eitherMsgs of
    Left e -> do
      tellError e
      replyTo m Nothing "Could not fetch messages from #potluck-vote"
    Right msgs -> do
      let nonBotMsgs = filter (not . userIsBot . messageAuthor) msgs
          emojiOrder = ["Bea", "Dre", "Fas", "Fri", "Hea", "Lev", "Lov", "Lur", "Moo", "Saf", "Spo"]
      voteRows <- forM nonBotMsgs $ \msg ->
        do
          let pad1 n
                | n < 10 = " " <> tshow n
                | otherwise = tshow n -- Assume we don't go above 100 per ball
          let pad2 n
                | n < 10 = "   " <> tshow n
                | n < 100 = "  " <> tshow n
                | otherwise = " " <> tshow n -- Assume we don't go above 1000 in total
          let text = messageContent msg
          authorServerNick <- getUserNickFromMessage msg (Just guildId)
          let msgReactions = M.fromList $ mapMaybe extractValidReactionsWithCount (messageReactions msg)
              totalVotes = sum $ M.elems msgReactions
              ballVotes = map (\e -> pad1 . fromMaybe 0 . M.lookup e $ msgReactions) emojiOrder
          pure (totalVotes, text : authorServerNick : pad2 totalVotes : ballVotes)
      let sortedVoteRows = map snd $ sortOn (Down . fst) voteRows
          headerRow = "Pokemon" : "Proposed by" : "Total" : emojiOrder
          table = makeTable (headerRow : sortedVoteRows) True (Just 5)
      replyWithFile
        m
        ("Current status of proposals in " <> "<#" <> tshow cid <> ">:")
        "potluck-votes.txt"
        table

respondPotluckSignup :: Message -> App DiscordHandler ()
respondPotluckSignup m = withContext "respondPotluckSignup" $ do
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
                  Just eid -> emojiName e <> ":" <> tshow eid -- Custom emoji
            users <- lift $ restCall $ DR.GetReactions (plSignupChannelId, messageId latestMsg) emoji (100, DR.FirstUsers)
            case users of
              Right users' -> pure $ Just (simpleName, users')
              Left _ -> do
                tellError e
                pure Nothing
          Nothing -> do
            tellError e
            pure Nothing
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

makeThread :: Message -> App DiscordHandler ()
makeThread m = withContext "makeThread" $ do
  cfg <- ask
  case messageReference m of
    Nothing -> replyTo m Nothing "You should use `!thread` when replying to your trading partner's message"
    Just ref -> do
      case (referenceChannelId ref, referenceMessageId ref) of
        (Just rcid, Just rmid) -> do
          eitherRepliedMsg <- lift $ restCall $ DR.GetChannelMessage (rcid, rmid)
          case eitherRepliedMsg of
            Left err1 -> do
              tellError err1
              replyTo m Nothing "Could not get info about the message you replied to."
            Right repliedMsg -> do
              let authorId1 = userId (messageAuthor m)
                  authorId2 = userId (messageAuthor repliedMsg)
              author1 <- getUserNickFromMessage m (messageGuildId m)
              author2 <- getUserNickFromMessage repliedMsg (messageGuildId m)
              eitherFirstMsg <- lift $ restCall $ DR.GetChannelMessages rcid (1, DR.AfterMessage (mkId 0))
              case eitherFirstMsg of
                Left err2 -> do
                  tellError err2
                  replyTo m Nothing "Could not get info about the first message in the channel."
                Right [firstMsg] -> do
                  atomically $ print (messageAuthor firstMsg)
                  when (authorId1 == userId (messageAuthor firstMsg)) $ do
                    when (authorId1 == authorId2) $ replyTo m Nothing "You can't trade with yourself!"
                    when (authorId1 /= authorId2) $ do
                      eitherChannel <- lift $ restCall $ DR.GetChannel rcid
                      case eitherChannel of
                        Left err3 -> do
                          tellError err3
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
                                Left err4 -> do
                                  tellError err4
                                  replyTo m Nothing "Could not create thread."
                                Right newThread -> do
                                  replyTo m (Just [authorId1, authorId2]) $ "Created thread for <@" <> tshow authorId1 <> "> and <@" <> tshow authorId2 <> "> at: " <> getChannelUrl newThread
                                  restCall_ $
                                    DR.CreateMessageDetailed (channelId newThread) $
                                      def
                                        { DR.messageDetailedContent = "Original post by <@" <> tshow authorId1 <> ">: https://discord.com/channels/" <> tshow (channelGuild chn) <> "/" <> tshow rcid <> "/" <> tshow (messageId firstMsg),
                                          DR.messageDetailedAllowedMentions = Just $ mentionOnly [authorId1]
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
                                          DR.messageDetailedAllowedMentions = Just $ mentionOnly [authorId2]
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
closeThread m = withContext "closeThread" $ do
  cfg <- ask
  let channelId = messageChannelId m
  eitherChannel <- lift $ restCall $ DR.GetChannel channelId
  case eitherChannel of
    Left err -> do
      tellError err
      replyTo m Nothing "Could not get info about the channel you replied in."
    Right chn@(ChannelPublicThread {}) -> do
      atomically $ print chn
      eitherFirstMsg <- lift $ restCall $ DR.GetChannelMessages channelId (1, DR.AfterMessage (mkId 0))
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
respondHelp m = withContext "respondHelp" $ do
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
        "- `!legality {pokemon}`",
        "  Show ball legality for a Pokémon across all available games.",
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
    [ ("beastball", mkId 1142586472751444108),
      ("dreamball", mkId 1142587632111595603),
      ("fastball", mkId 1142587859212193793),
      ("friendball", mkId 1142588024518103110),
      ("heavyball", mkId 1142588347236237384),
      ("levelball", mkId 1142589194523377674),
      ("loveball", mkId 1142589700725547112),
      ("lureball", mkId 1142589833395589230),
      ("moonball", mkId 1142589959879000136),
      ("safariball", mkId 1142590142515785840),
      ("sportball", mkId 1142590314364817508)
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
    Left e -> do
      tellError e
    Right _ -> pure ()

replyTo :: Message -> Maybe [UserId] -> Text -> App DiscordHandler ()
replyTo m allowedMentions txt =
  withContext "replyTo" $
    restCall_ $
      DR.CreateMessageDetailed
        (messageChannelId m)
        ( def
            { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
              DR.messageDetailedContent = txt,
              DR.messageDetailedAllowedMentions = mentionOnly <$> allowedMentions
            }
        )

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
getUserNickFromMessage m maybeGid = withContext "getUserNickFromMessage" $ do
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
getUserNick u maybeGid = withContext "getUserNick" $ do
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
replyWithFile m msgContents fname fcontents = withContext "replyWithFile" $ do
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

tellError :: (Show e) => e -> App DiscordHandler ()
tellError e = notifyDiscord (Log $ "Error: " <> tshow e)