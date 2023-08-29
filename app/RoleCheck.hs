-- | Module which checks that everybody in the server who has reacted with
-- cherishball to the welcome message has the verified role.
module RoleCheck (roleCheck) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Discord
import qualified Discord.Requests as DR
import Discord.Types
import System.Exit (exitSuccess)
import Trans

-- | Run the Discord bot.
roleCheck :: App IO ()
roleCheck = do
  cfg <- ask
  err <-
    liftIO $
      runDiscord $
        def
          { discordToken = cfgDiscordToken cfg,
            discordOnEvent = const $ pure (),
            discordOnStart = runAppWith cfg startup
          }
  atomically $ T.putStrLn err

-- | Perform the role check and exit
startup :: App DiscordHandler ()
startup = do
  let (welcomeChanId, welcomeMsgId) = (1120783101988180078, 1132810456474595429)
      welcomeEmote = ":cherishball:1132047633658163270"
  -- Get Cherish Ball reacts on main message
  users <- getAllReactions (welcomeChanId, welcomeMsgId) welcomeEmote DR.LatestReaction
  case users of
    Right us -> do
      print' $ T.replicate 50 "-"
      print' $ "Found " <> (T.pack . show $ length us) <> " cherishball reacts"
      print' $ T.replicate 50 "-"
      forM_ us $ \u -> do
        gid <- asks cfgAprimarketGuildId
        eitherGuildMember <- lift $ restCall $ DR.GetGuildMember gid (userId u)
        case eitherGuildMember of
          Left _ -> print' $ "*** WARNING: Failed to get guild member " <> userName u
          Right mem -> do
            print' $ userName u
            when
              (1131737932173152307 `notElem` memberRoles mem)
              (print' $ "*** ERROR: " <> userName u <> " does not have verified role")
    _ -> pure ()
  liftIO exitSuccess

-- | Recursively retrieve all reactions on a message
--
-- NOTE: This does not include super reactions
getAllReactions :: (ChannelId, MessageId) -> Text -> DR.ReactionTiming -> App DiscordHandler (Either RestCallErrorCode [User])
getAllReactions (chanId, msgId) emote timing = do
  users1 <- lift $ restCall $ DR.GetReactions (chanId, msgId) emote (100, timing)
  case users1 of
    Left err -> pure $ Left err
    Right users ->
      if length users < 100
        then pure $ Right users
        else do
          let lastUserId = userId $ last users
          remainingUsers <- getAllReactions (chanId, msgId) emote (DR.AfterReaction lastUserId)
          case remainingUsers of
            Left err -> pure $ Left err
            Right users' -> pure $ Right $ users <> users'

print' :: Text -> App DiscordHandler ()
print' = liftIO . T.putStrLn
