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
    getLatestHits,
    getLatestNonHits,
    wasHit,
    getTotalRows,
    getTotalMLAssignedRows,
    getTotalMLAssignedHits,
    getTotalNumberLabelled,
    getNumberLabelledBy,
    getNextUnlabelledPost,
    addVote,
    getNumVotes,
    getLastNVotesBy,
    addNotifiedPost,
    checkNotifiedStatus,
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
import Discord.Types (ChannelId, DiscordId (..), MessageId, unId, unSnowflake)
import Reddit
import Reddit.Auth (Token (..), parseScopes, showScopes)
import Trans

-- | Add a post to the `posts` table. The Bool parameter indicates whether it
-- was a hit or not.
addToDb :: (MonadIO m) => Post -> Bool -> App m ()
addToDb post hit = withAppPsqlConn $ \conn ->
  void $
    execute
      conn
      [sql|INSERT INTO posts (id, url, title, body, submitter, utc_time, flair, hit)
       VALUES (?,?,?,?,?,?,?,?)
       ON CONFLICT (id) DO NOTHING;|]
      ( unPostID (postId post),
        postUrl post,
        postTitle post,
        postBody post,
        postAuthor post,
        postCreatedTime post,
        postFlairText post,
        hit
      )

getLatestHits :: (MonadIO m) => Int -> App m [(Text, Text, Text, Text, UTCTime, Maybe Text)]
getLatestHits n = withAppPsqlConn $ \conn ->
  query
    conn
    [sql|SELECT id, url, title, submitter, utc_time, flair FROM posts
         WHERE hit
         ORDER BY utc_time DESC
         LIMIT ?;|]
    (Only n)

getLatestNonHits :: (MonadIO m) => Int -> App m [(Text, Text, Text, Text, UTCTime, Maybe Text)]
getLatestNonHits n = withAppPsqlConn $ \conn ->
  query
    conn
    [sql|SELECT id, url, title, submitter, utc_time, flair FROM posts
         WHERE NOT hit
         ORDER BY utc_time DESC
         LIMIT ?;|]
    (Only n)

wasHit :: (MonadIO m) => Text -> App m Bool
wasHit postId = withAppPsqlConn $ \conn -> do
  hits <- query conn "SELECT hit FROM posts WHERE id = ?;" (Only postId)
  case hits of
    [] -> pure False
    (h : _) -> pure (fromOnly h)

getTotalRows :: (MonadIO m) => App m Int
getTotalRows = withAppPsqlConn $ \conn -> do
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM posts;"

getTotalMLAssignedRows :: (MonadIO m) => App m Int
getTotalMLAssignedRows = withAppPsqlConn $ \conn -> do
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM posts WHERE hit IS NOT NULL;"

getTotalMLAssignedHits :: (MonadIO m) => App m Int
getTotalMLAssignedHits = withAppPsqlConn $ \conn -> do
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM posts WHERE hit;"

getTotalNumberLabelled :: (MonadIO m) => App m Int
getTotalNumberLabelled = withAppPsqlConn $ \conn -> do
  fromOnly . head
    <$> query_ conn "SELECT COUNT(DISTINCT post_id) FROM votes;"

getNumberLabelledBy :: (MonadIO m) => Text -> App m Int
getNumberLabelledBy username = withAppPsqlConn $ \conn -> do
  ns <- query conn "SELECT COUNT(*) FROM votes WHERE username = ?;" (Only username)
  case ns of
    [Only n] -> pure n
    _ -> error "getNumberLabelledBy: query returned more than one row"

-- | Returns post ID, URL, title, body, submitter, time, flair
getNextUnlabelledPost :: (MonadIO m) => App m (Maybe (Text, Text, Text, Text, Text, UTCTime, Maybe Text))
getNextUnlabelledPost = withAppPsqlConn $ \conn -> do
  unlabelled <-
    query_
      conn
      [sql|SELECT id, url, title, body, submitter, utc_time, flair
           FROM posts
           WHERE id NOT IN (SELECT post_id FROM votes)
           ORDER BY RANDOM()
           LIMIT 1;|]
  case unlabelled of
    [] -> pure Nothing
    (p : _) -> pure (Just p)

-- | Record a user's vote on a post
addVote :: (MonadIO m) => Text -> Text -> Bool -> App m ()
addVote postId voter vote = withAppPsqlConn $ \conn -> do
  void $
    execute
      conn
      "INSERT INTO votes (post_id, username, vote) VALUES (?,?,?)"
      (postId, voter, vote)

getNumVotes :: (MonadIO m) => App m Int
getNumVotes = withAppPsqlConn $ \conn -> do
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM votes;"

-- | Get the most recent @count@ votes by a given user. Returned in descending
-- order of time, i.e. most recent votes first.
getLastNVotesBy :: (MonadIO m) => Int -> Text -> App m [(Text, Text, Text, Text, Bool)]
getLastNVotesBy count username = withAppPsqlConn $ \conn -> do
  query
    conn
    [sql|SELECT v.post_id, p.title, p.url, p.submitter, v.vote
         FROM posts as p
         INNER JOIN votes as v ON p.id = v.post_id
         WHERE v.username = ?
         ORDER BY v.id DESC
         LIMIT ?|]
    (username, count)

addNotifiedPost :: (MonadIO m) => ID Post -> ChannelId -> MessageId -> App m ()
addNotifiedPost postId channelId messageId = withAppPsqlConn $ \conn -> do
  void $
    execute
      conn
      "INSERT INTO discord (post_id, channel_id, message_id) VALUES (?,?,?)"
      ( unPostID postId,
        T.pack . show . unSnowflake . unId $ channelId,
        T.pack . show . unSnowflake . unId $ messageId
      )

checkNotifiedStatus :: (MonadIO m) => ID Post -> App m (Maybe (ChannelId, MessageId))
checkNotifiedStatus postId = withAppPsqlConn $ \conn -> do
  notified <-
    query
      conn
      "SELECT channel_id, message_id FROM discord WHERE post_id = ?"
      (Only (unPostID postId))
  case notified of
    [] -> pure Nothing
    (channelId, messageId) : _ ->
      pure $ Just (DiscordId . read . T.unpack $ channelId, DiscordId . read . T.unpack $ messageId)

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
