{-# LANGUAGE QuasiQuotes #-}

-- | Database containing Reddit posts, as well as community votes on the posts.
--
-- The schema for the 'posts' database is as follows:
--    - id         : Post ID. This is the primary key.
--    - url        : Post URL.
--    - title      : Post title.
--    - body       : Post body.
--    - submitter  : Post submitter.
--    - hit        : 1 or 0, indicates whether the bot reported it or not.
--                   If null, the bot didn't process it.
--    - utc_time   : Post creation time in UTC.
--    - flair      : Post flair text. Empty if unflaired.
--
-- The schema for the 'votes' database is as follows:
--    - post_id    : Post ID.
--    - username   : The Reddit username of the person who voted (no \/u\/ prefix).
--    - vote       : 1 if the user says it is Aprimon-related. 0 if not.
--
-- The tokens are stored in data/tokens.db. The schema is:
--
--    - id         : Text, primary key. This is the value stored in the user's
--                   cookie, which we use to look up their token
--    The next four columns are direct serialisations of the Reddit.Auth.Token
--    type.
--    - token      : Text
--    - token_type : Text
--    - expires_at : Text
--    - scopes     : Text
module Database
  ( addToDb,
    HitStatus (..),
    addNotifiedPost,
    checkNotifiedStatus,
    wasHit,
    addVote,
    addToken,
    getToken,
    removeToken,
    updateToken,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime (..))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Discord.Types (DiscordId (..), MessageId, unId, unSnowflake)
import Reddit
import Reddit.Auth (Token (..), parseScopes, showScopes)
import Trans

data HitStatus = DefinitelyNegative | Negative | Positive deriving (Eq, Show)

-- | Add a post to the `posts` table.
addToDb :: (MonadIO m) => Post -> HitStatus -> App m ()
addToDb post hit = withAppPsqlConn $ \conn ->
  void $
    execute
      conn
      [sql|INSERT INTO posts (id, url, title, body, submitter, utc_time, flair, hit, needs_review)
       VALUES (?,?,?,?,?,?,?,?,?)
       ON CONFLICT (id) DO NOTHING;|]
      ( unPostID (postId post),
        postUrl post,
        postTitle post,
        postBody post,
        postAuthor post,
        postCreatedTime post,
        postFlairText post,
        hit == Positive,
        hit /= DefinitelyNegative
      )

wasHit :: (MonadIO m) => Text -> App m Bool
wasHit postId = withAppPsqlConn $ \conn -> do
  hits <- query conn "SELECT hit FROM posts WHERE id = ?;" (Only postId)
  case hits of
    [] -> pure False
    (h : _) -> pure (fromOnly h)

-- | Record a user's vote on a post
addVote :: (MonadIO m) => Text -> Text -> Bool -> App m ()
addVote postId voter vote = withAppPsqlConn $ \conn -> do
  void $
    execute
      conn
      "INSERT INTO votes (post_id, username, vote) VALUES (?,?,?)"
      (postId, voter, vote)

addNotifiedPost :: (MonadIO m) => ID Post -> MessageId -> App m ()
addNotifiedPost postId messageId = withAppPsqlConn $ \conn -> do
  void $
    execute
      conn
      "INSERT INTO discord (post_id, message_id) VALUES (?,?)"
      ( unPostID postId,
        T.pack . show . unSnowflake . unId $ messageId
      )

checkNotifiedStatus :: (MonadIO m) => ID Post -> App m (Maybe MessageId)
checkNotifiedStatus postId = do
  notified <- withAppPsqlConn $ \conn -> do
    query
      conn
      "SELECT message_id FROM discord WHERE post_id = ?"
      (Only (unPostID postId))
  atomically $ print notified
  case notified of
    [] -> pure Nothing
    Only messageId : _ ->
      pure $ Just $ DiscordId . read . T.unpack $ messageId

-- tokens.db
serialiseToken :: Token -> (Text, Text, UTCTime, Text, Text)
serialiseToken tkn =
  ( decodeUtf8 $ token tkn,
    decodeUtf8 $ tokenType tkn,
    tokenExpiresAt tkn,
    showScopes $ tokenScopes tkn,
    fromMaybe "" (tokenRefreshToken tkn)
  )

deserialiseToken :: (Text, Text, UTCTime, Text, Text) -> Token
deserialiseToken (token, tokenType, expiresAt, scopes, refresh_token) =
  Token
    { token = encodeUtf8 token,
      tokenType = encodeUtf8 tokenType,
      tokenExpiresAt = expiresAt,
      tokenScopes = parseScopes scopes,
      tokenRefreshToken = case refresh_token of
        "" -> Nothing
        _ -> Just refresh_token
    }

addToken :: (MonadIO m) => Text -> Token -> App m ()
addToken identifier tkn = withAppPsqlConn $ \conn -> do
  let (token, tokenType, expiresAt, scopes, refreshToken) = serialiseToken tkn
  void $
    execute
      conn
      [sql|INSERT INTO tokens (id, token, token_type, expires_at, scopes, refresh_token)
         VALUES (?,?,?,?,?,?)
         ON CONFLICT(id)
         DO UPDATE SET token = EXCLUDED.token, token_type = EXCLUDED.token_type, expires_at = EXCLUDED.expires_at, scopes = EXCLUDED.scopes, refresh_token = EXCLUDED.refresh_token;|]
      ( identifier,
        token,
        tokenType,
        expiresAt,
        scopes,
        refreshToken
      )

getToken :: (MonadIO m) => Text -> App m (Maybe Token)
getToken identifier = withAppPsqlConn $ \conn -> do
  tokens <-
    query
      conn
      "SELECT token, token_type, expires_at, scopes, refresh_token FROM tokens WHERE id = ?"
      (Only identifier)
  pure $ case tokens of
    [] -> Nothing
    (t : _) -> Just $ deserialiseToken t

removeToken :: (MonadIO m) => Text -> App m ()
removeToken identifier = withAppPsqlConn $ \conn -> do
  void $ execute conn "DELETE FROM tokens WHERE id = ?" (Only identifier)

updateToken :: (MonadIO m) => Text -> Token -> App m ()
updateToken identifier tkn = withAppPsqlConn $ \conn -> do
  let (token, tokenType, expiresAt, scopes, _) = serialiseToken tkn
  void $
    execute
      conn
      "UPDATE tokens SET token = ?, token_type = ?, expires_at = ?, scopes = ? WHERE id = ?;"
      (token, tokenType, expiresAt, scopes, identifier)
