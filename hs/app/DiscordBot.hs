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

import Apripsql.Queries (DBPokemon (..), GetPokemonResult (..))
import qualified Apripsql.Queries as Q
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Char.WCWidth (wcwidth)
import Data.List (nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Word (Word64)
import Database (addNotifiedPost, checkNotifiedStatus)
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import Parser (DiscordCommand (..), parseDiscordCommand)
import Pokemon
import Reddit (ID (..), Post (..), getPost, runRedditT)
import qualified Setup.EggGroup as EggGroup
import Setup.Game (Game (..))
import Setup.GenderRatio (GenderRatio (..))
import qualified Setup.Type as Type
import System.Process (readProcess)
import System.Random (randomRIO)
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
  cfg <- ask
  -- Print if it's not a GuildCreate
  case e of
    GuildCreate {} -> pure ()
    MessageCreate m -> atomically $ T.putStrLn $ "MessageCreate by " <> userName (messageAuthor m) <> ": " <> messageContent m
    _ -> atomically $ print e
  case e of
    GuildCreate {} -> pure ()
    MessageReactionAdd rinfo -> do
      case cfgRoleReactionsMessageId cfg of
        Just mid -> do
          when (reactionMessageId rinfo == mid) $ do
            addRole (reactionGuildId rinfo) (reactionUserId rinfo) (reactionEmoji rinfo)
        Nothing -> pure ()
    MessageCreate m -> do
      -- Only respond if deployed, or the message is from myself
      when (cfgOnFly cfg || userId (messageAuthor m) == cfgPennyId cfg) $ do
        let content = T.strip (messageContent m)
        when ("!" `T.isPrefixOf` content) $ do
          let cmd = parseDiscordCommand content
          logToDiscord (tshow cmd)
          case cmd of
            Nothing -> pure ()
            Just Help -> respondHelp m
            Just Thread -> makeThread m
            Just CloseThread -> closeThread m
            Just PotluckVotes -> respondPotluckVotes m
            Just PotluckSignup -> respondPotluckSignup m
            Just Sandwich -> respondSandwich m
            Just (Info pkmnName) -> respondInfo m pkmnName
            Just (HA pkmnName) -> respondHA m pkmnName
            Just (EM game pkmnName) -> respondEM False m game pkmnName
            Just (EMParents game pkmnName) -> respondEM True m game pkmnName
            Just (Nature pkmnName) -> respondNature m pkmnName
            Just (Legality pkmnName) -> respondLegality m pkmnName
            Just (Sprite pkmnName) -> respondSprite m pkmnName
        when
          ( cfgApribotId cfg `elem` map userId (messageMentions m)
              && userId (messageAuthor m) == cfgPennyId cfg
              && messageChannelId m /= cfgR2DChannelId cfg
          )
          $ runLLM m >>= replyTo m Nothing
    -- Ignore other events (for now)
    _ -> pure ()

-- | This function is exported to allow the Reddit bot to talk to this module.
-- It adds the post to the MVar, which effectively triggers channelLoop to post
-- a message to Discord.
notifyDiscord :: (MonadIO m) => NotifyEvent -> App m ()
notifyDiscord e = do
  -- If e is a Log or Debug event, prefix it with the current execution context
  e' <- case e of
    Log msg -> do
      ctx <- asks cfgContext
      pure $ Log $ ctx <> ": " <> msg
    Debug msg -> do
      ctx <- asks cfgContext
      pure $ Debug $ "[debug] " <> ctx <> ": " <> msg
    _ -> pure e
  chan <- asks cfgChan
  liftIO $ writeChan chan e'

-- Post a log event to #apribot-logs in my server. The Text value that it's
-- called with must contain all relevant info, none of it is added by this
-- function
logToDiscord :: Text -> App DiscordHandler ()
logToDiscord msg = do
  cfg <- ask
  restCall_ $ DR.CreateMessage (cfgLogChannelId cfg) msg

debugToDiscord :: Text -> App DiscordHandler ()
debugToDiscord msg = do
  cfg <- ask
  restCall_ $ DR.CreateMessage (cfgDebugChannelId cfg) msg

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
        now <- liftIO getCurrentTime
        let isInLastTwoDays = diffUTCTime now (postCreatedTime post) > 60 * 60 * 24 * 2
        when (isNothing alreadyNotified && isInLastTwoDays) $ do
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
                      (messageId msg)
  -- Delete the post (if it's not supposed to be there)
  let unnotifyPost :: ID Post -> App DiscordHandler ()
      unnotifyPost pid = do
        notified <- checkNotifiedStatus pid
        chanId <- asks cfgPtrChannelId
        case notified of
          Nothing -> pure ()
          Just msgId -> restCall_ $ DR.DeleteMessage (chanId, msgId)
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
      Debug t -> debugToDiscord t

createEmEmbedDescription :: Q.EggMoveParents -> Text
createEmEmbedDescription em' =
  let levelUpParents =
        case mapMaybe
          ( \case
              Q.LevelUpParent nm lvl -> Just $ nm <> " (" <> tshow lvl <> ")"
              Q.EvolutionParent nm -> Just $ nm <> " (evolution)"
              Q.BreedParent {} -> Nothing
          )
          (Q.empParents em') of
          [] -> []
          xs -> ["**Parents which learn by level up**\n" <> T.intercalate ", " xs]
      breedParents =
        case mapMaybe
          ( \case
              Q.BreedParent nm -> Just nm
              _ -> Nothing
          )
          (Q.empParents em') of
          [] -> []
          xs -> ["**Parents which learn by breeding**\n" <> T.intercalate ", " xs]
   in T.intercalate "\n\n" (Q.emFlavorText (Q.empMove em') : levelUpParents <> breedParents)

suggestWebsite :: Text
suggestWebsite = "If you're having trouble finding a Pokémon, try looking it up at https://apribot.fly.dev/names"

didYouMean :: NonEmpty Text -> Text
didYouMean xs =
  let bolden t = "**" <> t <> "**"
      didYouMeanSentence = case xs of
        x :| [] -> "Did you mean: " <> bolden x <> "?"
        _ -> "Did you mean any of these: " <> T.intercalate ", " (map bolden $ NE.toList xs) <> "?"
   in T.unwords [didYouMeanSentence, suggestWebsite]

guessingYouMeant :: Q.DBPokemon -> NonEmpty Text -> Text
guessingYouMeant sp uniqueNames =
  let italicise t = "*" <> t <> "*"
   in "I'm guessing you meant **"
        <> mkFullName (dbName sp) (dbForm sp)
        <> "**. The possibilities were: "
        <> T.intercalate ", " (NE.toList $ NE.map italicise uniqueNames)
        <> ".\n\n"

parenthesise :: Text -> Text
parenthesise t = "\n\n(" <> t <> ")"

respondNature :: Message -> Maybe Text -> App DiscordHandler ()
respondNature m mPkmn = withContext ("respondNature (`" <> messageContent m <> "`)") $ do
  case mPkmn of
    Nothing -> replyTo m Nothing "usage: `!nature {pokemon}` (e.g. `!nature togepi`)"
    Just pkmn -> do
      -- Try to fetch the Pokemon first.
      pkmnDetails <- withAppPsqlConn $ Q.getPokemon pkmn
      case pkmnDetails of
        NoneFound ->
          replyTo m Nothing $
            T.unwords
              [ "No Pokémon with name '" <> pkmn <> "' found.",
                parenthesise suggestWebsite
              ]
        NoneFoundButSuggesting uniqueNames ->
          replyTo m Nothing $
            T.unwords
              [ "No Pokémon with name '" <> pkmn <> "' found.",
                parenthesise $ didYouMean uniqueNames
              ]
        AliasedToAndSuggesting sp uniqueNames ->
          replyWithSuggestedNatures (guessingYouMeant sp uniqueNames) sp
        FoundOne sp ->
          replyWithSuggestedNatures "" sp
      where
        replyWithSuggestedNatures :: Text -> Q.DBPokemon -> App DiscordHandler ()
        replyWithSuggestedNatures messagePrefix sp = do
          let pkmnId = dbId sp
              pkmnName = dbName sp
              pkmnForm = dbForm sp
          let fullName = mkFullName pkmnName pkmnForm
          suggestedNatures <- getSuggestedNatures pkmnId
          case suggestedNatures of
            Nothing -> replyTo m Nothing $ messagePrefix <> "No suggested natures found for " <> fullName <> "."
            Just sn -> do
              let text =
                    messagePrefix
                      <> "Suggested natures for "
                      <> fullName
                      <> ":"
                      <> case penny sn of
                        Nothing -> ""
                        Just n -> "\n- Penny's sheet (mostly Pikalytics): " <> n
                      <> case jemmaSwSh sn of
                        Nothing -> ""
                        Just n -> "\n- Jemma's SwSh sheet (Smogon): " <> n
                      <> case jemmaBDSP sn of
                        Nothing -> ""
                        Just n -> "\n- Jemma's BDSP sheet (Smogon): " <> n
                      <> case jemmaG7 sn of
                        Nothing -> ""
                        Just n -> "\n- Jemma's G7 sheet (Smogon): " <> n
              replyTo m Nothing text

giveRandomEMs :: Message -> Text -> Maybe (NonEmpty Text) -> App DiscordHandler ()
giveRandomEMs m requestedPokemon suggestedUniqueNames = do
  n <- liftIO $ randomRIO (2, 6)
  moveDescs <- sort <$> withAppPsqlConn (Q.randomMoves n)
  let messageText =
        T.unwords
          [ "I don't think " <> requestedPokemon <> " is a Pokemon, but if it was, it would have the egg moves: ",
            T.intercalate ", " (map fst moveDescs) <> "!",
            parenthesise $ maybe suggestWebsite didYouMean suggestedUniqueNames
          ]
  let embed =
        def
          { createEmbedTitle = "Descriptions",
            createEmbedDescription =
              T.intercalate
                "\n"
                (zipWith (\i (m', d') -> tshow i <> ". **" <> m' <> "**: " <> d') [(1 :: Int) ..] moveDescs),
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

respondEM :: Bool -> Message -> Maybe Game -> Maybe Text -> App DiscordHandler ()
respondEM withParents m mGame mPkmn = withContext ("respondEM (`" <> messageContent m <> "`)") $ do
  let cmd = if withParents then "!emp" else "!em"
  let replyWithUsage = replyTo m Nothing $ "usage: `" <> cmd <> " {game} {pokemon}` ({game} is usum, bdsp, swsh, or sv). e.g. `" <> cmd <> " swsh togepi`"
  case (mGame, mPkmn) of
    -- If message contents weren't parsed properly, prompt the user
    (Nothing, _) -> replyWithUsage
    (_, Nothing) -> replyWithUsage
    -- Otherwise, try to fetch the Pokemon
    (Just game, Just pkmn) -> do
      -- Try to fetch the Pokemon first. If it can't be found, choose some random moves
      pkmnDetails <- withAppPsqlConn $ Q.getPokemon pkmn
      case pkmnDetails of
        NoneFound -> giveRandomEMs m pkmn Nothing
        NoneFoundButSuggesting uniqueNames -> giveRandomEMs m pkmn (Just uniqueNames)
        AliasedToAndSuggesting sp uniqueNames -> do
          replyWithEMs (guessingYouMeant sp uniqueNames) withParents sp game
        FoundOne sp ->
          replyWithEMs "" withParents sp game
  where
    replyWithEMs :: Text -> Bool -> Q.DBPokemon -> Game -> App DiscordHandler ()
    replyWithEMs messagePrefix withParents' sp game = do
      let id' = dbId sp
      let fullName = mkFullName (dbName sp) (dbForm sp)
      if withParents'
        then do
          ems <- sort <$> withAppPsqlConn (Q.getEMParents game id')
          case ems of
            -- No egg moves
            [] ->
              replyTo m Nothing $ messagePrefix <> fullName <> " has no egg moves in " <> tshow game
            -- Egg moves
            ems' -> do
              let makeEmEmbedWithParents :: Int -> Q.EggMoveParents -> CreateEmbed
                  makeEmEmbedWithParents n emp =
                    def
                      { createEmbedTitle = "" <> tshow n <> ". " <> Q.emName (Q.empMove emp),
                        createEmbedDescription = createEmEmbedDescription emp,
                        createEmbedColor = Just DiscordColorLuminousVividPink
                      }
              let emText =
                    messagePrefix
                      <> fullName
                      <> " egg moves in "
                      <> tshow game
                      <> ": "
                      <> T.intercalate ", " (map (Q.emName . Q.empMove) ems')
              let embeds = zipWith makeEmEmbedWithParents [1 :: Int ..] ems'
              let postSubsequentEms remainingEmbeds =
                    case splitAt 3 remainingEmbeds of
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
              case splitAt 3 embeds of
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
          -- Parents not requested
          ems <- sort <$> withAppPsqlConn (Q.getEMs game id')
          case ems of
            -- No egg moves
            [] -> do
              replyTo m Nothing $ messagePrefix <> fullName <> " has no egg moves in " <> tshow game
            -- Egg moves
            ems' -> do
              let emText =
                    messagePrefix
                      <> fullName
                      <> " egg moves in "
                      <> tshow game
                      <> ": "
                      <> T.intercalate ", " (map Q.emName ems')
                      <> "\nFor full details about parents, use `!emp "
                      <> T.toLower (tshow game)
                      <> " "
                      <> dbUniqueName sp
                      <> "` instead."
              let emEmbedShort =
                    def
                      { createEmbedTitle = "Descriptions",
                        createEmbedDescription =
                          T.intercalate "\n" $
                            zipWith (\n e -> tshow n <> ". **" <> Q.emName e <> "**: " <> Q.emFlavorText e) [1 :: Int ..] ems',
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

makeHAEmbed :: (Text, Text) -> Maybe CreateEmbed
makeHAEmbed (ha', desc') =
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

giveRandomHA :: Message -> Text -> Maybe (NonEmpty Text) -> App DiscordHandler ()
giveRandomHA m requestedPokemon suggestedUniqueNames = do
  (ha', desc') <- withAppPsqlConn Q.randomAbility
  restCall_ $
    DR.CreateMessageDetailed
      (messageChannelId m)
      ( def
          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
            DR.messageDetailedContent =
              T.unwords
                [ "I don't think " <> requestedPokemon <> " is a Pokemon, but if it was, it would have the hidden ability " <> ha' <> "!",
                  parenthesise $ maybe suggestWebsite didYouMean suggestedUniqueNames
                ],
            DR.messageDetailedAllowedMentions = Nothing,
            DR.messageDetailedEmbeds = (: []) <$> makeHAEmbed (ha', desc')
          }
      )

respondHA :: Message -> Maybe Text -> App DiscordHandler ()
respondHA m mPkmn = withContext ("respondHA (`" <> messageContent m <> "`)") $ do
  case mPkmn of
    Nothing -> replyTo m Nothing "usage: `!ha {pokemon}` (e.g. `!ha togepi`)"
    Just pkmn -> do
      -- Try to fetch the Pokemon first. If it can't be found, choose some random moves
      pkmnDetails <- withAppPsqlConn $ Q.getPokemon pkmn
      case pkmnDetails of
        NoneFound ->
          giveRandomHA m pkmn Nothing
        NoneFoundButSuggesting uniqueNames ->
          giveRandomHA m pkmn (Just uniqueNames)
        AliasedToAndSuggesting sp uniqueNames ->
          replyWithRealHA (guessingYouMeant sp uniqueNames) sp
        FoundOne sp ->
          replyWithRealHA "" sp
  where
    replyWithRealHA :: Text -> Q.DBPokemon -> App DiscordHandler ()
    replyWithRealHA messagePrefix sp = do
      let fullName = mkFullName (dbName sp) (dbForm sp)
      let piplupNote = "(Note that prior to SV, the Piplup family had Defiant as their HA.)"
          allNotes :: Map Text Text
          allNotes =
            M.fromList
              [ ("Piplup", piplupNote),
                ("Prinplup", piplupNote),
                ("Empoleon", piplupNote),
                ("Ferroseed", "(Note that Ferrothorn has Anticipation as its HA.)")
              ]
          note = case M.lookup (dbName sp) allNotes of Just d -> "\n" <> d; Nothing -> ""
      case dbHiddenAbilityId sp of
        Nothing -> replyTo m Nothing $ messagePrefix <> fullName <> " has no hidden ability." <> note
        Just haId -> do
          (haName, haDesc) <- withAppPsqlConn $ Q.getAbility haId
          restCall_ $
            DR.CreateMessageDetailed
              (messageChannelId m)
              ( def
                  { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                    DR.messageDetailedContent = messagePrefix <> fullName <> "'s hidden ability is: " <> haName <> note,
                    DR.messageDetailedAllowedMentions = Nothing,
                    DR.messageDetailedEmbeds = (: []) <$> makeHAEmbed (haName, haDesc)
                  }
              )

respondInfo :: Message -> Maybe Text -> App DiscordHandler ()
respondInfo m mPkmn = withContext ("respondInfo (`" <> messageContent m <> "`)") $ do
  case mPkmn of
    Nothing -> replyTo m Nothing "usage: `!info {pokemon}` (e.g. `!info togepi`)"
    Just pkmn -> do
      -- Try to fetch the Pokemon first. If it can't be found, choose some random moves
      pkmnDetails <- withAppPsqlConn $ Q.getPokemon pkmn
      case pkmnDetails of
        -- Not a Pokemon
        NoneFound ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise suggestWebsite]
        NoneFoundButSuggesting uniqueNames ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise $ didYouMean uniqueNames]
        AliasedToAndSuggesting sp uniqueNames ->
          replyWithInfo (guessingYouMeant sp uniqueNames) sp
        FoundOne sp ->
          replyWithInfo "" sp
  where
    replyWithInfo :: Text -> Q.DBPokemon -> App DiscordHandler ()
    replyWithInfo messagePrefix sp = do
      let fullName = mkFullName (dbName sp) (dbForm sp)
      -- Typing
      let typing =
            let typeIdToText = T.pack . Type.toString . toEnum . pred
             in case (dbType1Id sp, dbType2Id sp) of
                  (t1, Nothing) -> typeIdToText t1
                  (t1, Just t2) -> typeIdToText t1 <> "/" <> typeIdToText t2
          typingText = "**Type** " <> typing
      -- Base stats
      let bsText =
            "**Base stats** "
              <> "HP "
              <> tshow (dbHp sp)
              <> " • Atk "
              <> tshow (dbAtk sp)
              <> " • Def "
              <> tshow (dbDef sp)
              <> " • SpA "
              <> tshow (dbSpa sp)
              <> " • SpD "
              <> tshow (dbSpd sp)
              <> " • Spe "
              <> tshow (dbSpe sp)
      -- Gender ratio
      let gr = toEnum . pred $ dbGenderRatioId sp
          grText =
            "**Gender ratio** " <> case gr of
              Genderless -> "Genderless"
              FemaleOnly -> ":female_sign: 100%"
              Female71 -> ":female_sign: 87.5% :male_sign: 12.5%"
              Female31 -> ":female_sign: 75% :male_sign: 25%"
              Equal -> ":female_sign: 50% :male_sign: 50%"
              Male31 -> ":female_sign: 25% :male_sign: 75%"
              Male71 -> ":female_sign: 12.5% :male_sign: 87.5%"
              MaleOnly -> ":male_sign: 100%"
      -- Egg groups
      let egs =
            let egToText = T.pack . EggGroup.toString . toEnum . pred
             in case (dbEggGroup1Id sp, dbEggGroup2Id sp) of
                  (eg1, Nothing) -> egToText eg1
                  (eg1, Just eg2) -> egToText eg1 <> ", " <> egToText eg2
          egText = "**Egg groups** " <> egs
      -- Egg cycles
      let ecText = "**Egg cycles** " <> tshow (dbEggCycles sp)
      -- Abilities
      let getAbilityName aid = do
            abil <- withAppPsqlConn $ Q.getAbility aid
            pure $ fst abil
      abil1Name <- getAbilityName (dbAbility1Id sp)
      abil2Name <- mapM getAbilityName (dbAbility2Id sp)
      haName <- mapM getAbilityName (dbHiddenAbilityId sp)
      let abilText =
            "### Abilities\n"
              <> abil1Name
              <> maybe "" (", " <>) abil2Name
              <> maybe " *(no HA)*" (\a -> ", **" <> a <> " (HA)**") haName

      -- EMs
      let getEMNames game = do
            ems <- withAppPsqlConn $ Q.getEMs game (dbId sp)
            pure $ map Q.emName ems
      emsUsum <- getEMNames USUM
      emsSwsh <- getEMNames SwSh
      emsBdsp <- getEMNames BDSP
      emsSv <- getEMNames SV
      let makeEmTextIn (ems, game) = "**" <> game <> "** " <> T.intercalate ", " (sort ems)
      let emText =
            "### Egg moves\n"
              <> case filter
                (not . null . fst)
                [ (emsUsum, "USUM"),
                  (emsSwsh, "SwSh"),
                  (emsBdsp, "BDSP"),
                  (emsSv, "SV")
                ] of
                [] -> "None."
                emsAndGames -> T.intercalate "\n" (map makeEmTextIn emsAndGames)
      -- Legality
      legalities <- getLegality (dbId sp)
      unbreedable <- withAppPsqlConn $ Q.isPokemonUnbreedable (dbId sp)
      let legText = "### Legality\n" <> mkLegalityText legalities unbreedable
      -- Natures
      suggestedNatures <- getSuggestedNatures (dbId sp)
      let natureText =
            "### Suggested natures\n" <> case suggestedNatures of
              Nothing -> "None."
              Just sn ->
                do
                  case penny sn of
                    Nothing -> ""
                    Just n -> "\n**Penny** " <> n
                  <> case jemmaSwSh sn of
                    Nothing -> ""
                    Just n -> "\n**Jemma SwSh** " <> n
                  <> case jemmaBDSP sn of
                    Nothing -> ""
                    Just n -> "\n**Jemma BDSP** " <> n
                  <> case jemmaG7 sn of
                    Nothing -> ""
                    Just n -> "\n**Jemma G7** " <> n
      -- Put it all together
      let embed =
            def
              { createEmbedTitle = fullName <> " (#" <> tshow (dbNdex sp) <> ")",
                createEmbedDescription =
                  T.intercalate
                    "\n"
                    [ typingText,
                      bsText,
                      grText,
                      egText,
                      ecText,
                      abilText,
                      emText,
                      legText,
                      natureText
                    ],
                createEmbedUrl = "https://pokemondb.net/pokedex/" <> (T.replace " " "-" . T.toLower $ dbName sp),
                createEmbedColor = Just DiscordColorLuminousVividPink
              }
      restCall_ $
        DR.CreateMessageDetailed
          (messageChannelId m)
          ( def
              { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
                DR.messageDetailedContent = messagePrefix,
                DR.messageDetailedAllowedMentions = Nothing,
                DR.messageDetailedEmbeds = Just [embed]
              }
          )

mkLegalityText :: Map Game (Bool, GenLegality) -> Bool -> Text
mkLegalityText legalities unbreedable =
  let showLegality (GenLegality b d a s sp) =
        T.concat
          [ if b then "<:beastball:1132050100017959033>" else "",
            if d then "<:dreamball:1132050106200375416>" else "",
            if a then "<:fastball:1132050109073465414><:friendball:1132050111598436414><:heavyball:1132050112965775541><:levelball:1132050114765148260><:loveball:1132050117323661382><:lureball:1132050118481285220><:moonball:1132050120251281530>" else "",
            if s then "<:safariball:1132052412501344266>" else "",
            if sp then "<:sportball:1132050124823068752>" else ""
          ]
   in T.intercalate
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
                        then "No rare ball combos available"
                        else showLegality legality
            )
            (M.assocs legalities)
        )

respondLegality :: Message -> Maybe Text -> App DiscordHandler ()
respondLegality m mPkmn = withContext ("respondLegality (`" <> messageContent m <> "`)") $ do
  case mPkmn of
    Nothing -> replyTo m Nothing "usage: `!legality {pokemon}` (e.g. `!legality togepi`)"
    Just pkmn -> do
      -- Try to fetch the Pokemon first.
      pkmnDetails <- withAppPsqlConn $ Q.getPokemon pkmn
      case pkmnDetails of
        NoneFound ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise suggestWebsite]
        NoneFoundButSuggesting uniqueNames ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise $ didYouMean uniqueNames]
        AliasedToAndSuggesting sp uniqueNames ->
          replyWithLegality (guessingYouMeant sp uniqueNames) sp
        FoundOne sp ->
          replyWithLegality "" sp
  where
    replyWithLegality :: Text -> Q.DBPokemon -> App DiscordHandler ()
    replyWithLegality messagePrefix spkmn = do
      unbreedable <- withAppPsqlConn $ Q.isPokemonUnbreedable (dbId spkmn)
      let fullName = mkFullName (dbName spkmn) (dbForm spkmn)
      legalities <- getLegality (dbId spkmn)
      let message :: Text
          message =
            messagePrefix
              <> fullName
              <> " legality:\n"
              <> mkLegalityText legalities unbreedable
      replyTo m Nothing message

respondSprite :: Message -> Maybe Text -> App DiscordHandler ()
respondSprite m mPkmn = withContext ("respondSprite (`" <> messageContent m <> "`)") $ do
  case mPkmn of
    Nothing -> replyTo m Nothing "usage: `!sprite {pokemon}` (e.g. `!sprite togepi`)"
    Just pkmn -> do
      -- Try to fetch the Pokemon first.
      pkmnDetails <- withAppPsqlConn $ Q.getPokemonWithSameNdex pkmn
      case pkmnDetails of
        NoneFound ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise suggestWebsite]
        NoneFoundButSuggesting uniqueNames ->
          replyTo m Nothing $
            T.unwords ["No Pokémon with name '" <> pkmn <> "' found.", parenthesise $ didYouMean uniqueNames]
        AliasedToAndSuggesting result _ ->
          -- Should not happen (because getPokemonWithSameNdex does not return
          -- this constructor), but we can safely handle it here anyway
          replyWithSprite result
        FoundOne result ->
          replyWithSprite result
  where
    replyWithSprite :: Q.DBPokemon -> App DiscordHandler ()
    replyWithSprite sp = do
      let pkmnName = dbName sp
          pkmnForm = dbForm sp
      maybeSprite <- getSprites (dbNdex sp)
      case maybeSprite of
        Nothing -> replyTo m Nothing $ "No sprites found for " <> mkFullName pkmnName pkmnForm <> "."
        Just sprite -> replyWithImage m ("Sprites for " <> mkFullName pkmnName pkmnForm <> " (or any form with the same Dex number):") sprite

respondSandwich :: Message -> App DiscordHandler ()
respondSandwich m =
  withContext "respondSandwich" $
    replyTo m Nothing $
      T.intercalate
        "\n"
        [ "**4-star sandwiches**",
          "NOTE: all members of the Union Circle must make the sandwiches together",
          "",
          "6 Hamburger, 2 Cherry Tomatoes",
          "4 Curry Powder, 4 Marmalade",
          "",
          "OR",
          "",
          "6 Hamburger, 2 Kiwi",
          "4 Curry Powder, 1 Vinegar, 3 Cream Cheese",
          "",
          "**Sweet sandwich**",
          "1 Rice",
          "1 Whipped Cream",
          "",
          "**Salty sandwich**",
          "1 Bacon",
          "1 Ketchup",
          "",
          "**Sour sandwich**",
          "1 Kiwi",
          "1 Vinegar",
          "",
          "**Bitter sandwich**",
          "1 Hamburger",
          "1 Marmalade",
          "",
          "**Spicy sandwich**",
          "1 Jalapeño",
          "1 Chili Sauce"
        ]

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
                                        { DR.messageDetailedContent = quoteInThread (messageContent firstMsg),
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
                                        { DR.messageDetailedContent = quoteInThread (messageContent repliedMsg),
                                          DR.messageDetailedAllowedMentions = Just $ mentionOnly []
                                        }
                        _ -> replyTo m Nothing "You can only use this command within a forum thread."
                _ -> replyTo m Nothing "Could not get info about the first message in the channel."
        _ -> replyTo m Nothing "Could not get info about the message you replied to."

closeThread :: Message -> App DiscordHandler ()
closeThread m = withContext "closeThread" $ do
  let channelId = messageChannelId m
  eitherChannel <- lift $ restCall $ DR.GetChannel channelId
  case eitherChannel of
    Left err -> do
      tellError err
      replyTo m Nothing "Could not get info about the channel you replied in."
    Right chn@(ChannelPublicThread {}) -> _closeThread m chn
    Right chn@(ChannelPrivateThread {}) -> _closeThread m chn
    Right _ ->
      pure ()

_closeThread :: Message -> Channel -> App DiscordHandler ()
_closeThread m chn = withContext "_closeThread" $ do
  cfg <- ask
  eitherFirstMsg <- lift $ restCall $ DR.GetChannelMessages (channelId chn) (1, DR.AfterMessage (mkId 0))
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
            DR.ModifyChannel (channelId chn) $
              def
                { DR.modifyChannelName = Just $ truncateThreadTitle $ "[Closed] " <> fromMaybe "" (channelThreadName chn),
                  DR.modifyChannelThreadArchived = Just True,
                  DR.modifyChannelThreadLocked = Just True
                }
    _ -> replyTo m Nothing "Could not get the first message in the channel."

respondHelp :: Message -> App DiscordHandler ()
respondHelp m = withContext "respondHelp" $ do
  votesChan <- asks cfgPotluckVotesChannelId
  signupChan <- asks cfgPotluckSignupChannelId
  replyTo m Nothing $
    T.intercalate
      "\n"
      [ "**General commands**",
        "- `!help`",
        "  Show this message.",
        "- `!info {pokemon}`",
        "  Show a comprehensive overview of a Pokémon, including stats, moves, and abilities.",
        "- `!ha {pokemon}`",
        "  Show the hidden ability of a Pokémon",
        "- `!em {game} {pokemon}`",
        "  Show egg moves for a Pokémon in a game. `{game}` can be `usum`, `bdsp`, `swsh`, or `sv`.",
        "- `!emp {game} {pokemon}`",
        "  Same as `!em`, but also show potential parents.",
        "- `!legality {pokemon}`",
        "  Show ball legality for a Pokémon across all available games.",
        "- `!nature {pokemon}`",
        "  Show suggested natures for a Pokémon (collated from a couple of spreadsheets).",
        "- `!sprite {pokemon}`",
        "  Show regular and shiny sprites for a Pokémon.",
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

-- * LLM bits

replaceApriBot :: Text -> Text
replaceApriBot = T.replace "<@1130570595176812546>" "@ApriBot"

discordMessageToJson :: Message -> App DiscordHandler (Map Text Text)
discordMessageToJson m = do
  apribotId <- asks cfgApribotId
  guildId <- asks cfgAprimarketGuildId
  if userId (messageAuthor m) == apribotId
    then pure $ M.fromList [("role", "assistant"), ("content", replaceApriBot (messageContent m))]
    else do
      userNick <- getUserNickFromMessage m (Just guildId)
      pure $ M.fromList [("role", "user"), ("name", userNick), ("content", messageContent m)]

runLLM :: Message -> App DiscordHandler Text
runLLM m = do
  restCall_ $ DR.TriggerTypingIndicator (messageChannelId m)

  -- Construct reply chain
  let makeChain :: Message -> App DiscordHandler [Map Text Text]
      makeChain = fmap reverse . makeChain' []
        where
          makeChain' ct msg =
            -- Cut off context at 1 message (increase this to pass more context)
            if length ct == 1
              then pure ct
              else do
                thisContent <- discordMessageToJson msg
                restOfContent <- case messageReferencedMessage msg of
                  Nothing -> pure []
                  Just msg' -> do
                    -- Need to perform an API call to get the full message, because
                    -- referenced messages do not contain their own reference
                    fullMsg' <- lift $ restCall $ DR.GetChannelMessage (messageChannelId msg', messageId msg')
                    case fullMsg' of
                      Left e -> do
                        tellError e
                        pure []
                      Right msg'' -> makeChain' (thisContent : ct) msg''
                pure $ thisContent : restOfContent

  -- Construct JSON and send it to Python script
  json <- TL.unpack . TL.decodeUtf8 . A.encode <$> makeChain m
  atomically $ T.putStrLn $ T.pack json
  llmPath <- asks cfgLLMPath
  result <- liftIO $ T.pack <$> readProcess llmPath [] json
  pure $ T.strip result

-- * Helper functions

-- | For convenience
restCall_ :: (Request (r a), FromJSON a) => r a -> App (ReaderT DiscordHandle IO) ()
restCall_ req = do
  resp <- lift $ restCall req
  case resp of
    Left e -> do
      atomically $ T.putStrLn $ tshow e
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

quoteInThread :: Text -> Text
quoteInThread t =
  let quotedFullMessage = T.unlines . map ("> " <>) . T.lines $ t
   in if T.length quotedFullMessage <= 2000
        then quotedFullMessage
        else case T.lines . T.take 1992 $ quotedFullMessage of
          [] -> error "Not possible" -- There are at least 2000 characters!
          [x] -> x <> " [...]"
          xs -> T.unlines $ init xs ++ ["> [...]"]

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

replyWithImage :: Message -> Text -> ByteString -> App DiscordHandler ()
replyWithImage m msgContents imgBS = withContext "replyWithImage" $ do
  restCall_ $
    DR.CreateMessageDetailed
      (messageChannelId m)
      ( def
          { DR.messageDetailedReference = Just (def {referenceMessageId = Just (messageId m)}),
            DR.messageDetailedContent = msgContents,
            DR.messageDetailedAllowedMentions = Just (mentionOnly []),
            DR.messageDetailedEmbeds = Just [def {createEmbedImage = Just (CreateEmbedImageUpload imgBS)}]
          }
      )

tellError :: (Show e) => e -> App DiscordHandler ()
tellError e = notifyDiscord (Log $ "Error: " <> tshow e)
