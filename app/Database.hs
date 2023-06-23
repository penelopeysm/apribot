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
--    - time       : Post creation time in UTC.
--    - flair      : Post flair text. Empty if unflaired.
--    - truth      : 1 or 0, indicates whether the post was really of interest.
--                   This has to be manually determined by human volunteers (I
--                   think). If null, the truth has not been determined.
--
-- The schema for the 'votes' database is as follows:
--    - n          : A numeric id. This is the primary key and autoincrements.
--    - id         : Post ID.
--    - username   : The Reddit username of the person who voted (no \/u\/ prefix).
--    - vote       : 1 if the user says it is Aprimon-related. 0 if not.
module Database
  ( addToDb,
    getLatestHits,
    getLatestNonHits,
    getTotalRows,
    getTotalHits,
    getTotalNumberLabelled,
    getNumberLabelledBy,
    getNextUnlabelledPost,
    addVote,
    getNumVotes,
    getAllVotesBy,
  )
where

import Config
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Database.SQLite.Simple
import Paths_apribot (getDataFileName)
import Reddit
import Text.Printf (printf)
import Utils

-- | Add a post to the SQLite database. The Bool parameter indicates whether it
-- was a hit or not.
addToDb :: Post -> Bool -> IO ()
addToDb post hit = do
  sql <- getDataFileName (dbFileName config) >>= open
  executeNamed
    sql
    "INSERT INTO posts (id, url, title, body, submitter, hit, time, flair) \
    \ VALUES (:id, :url, :title, :body, :submitter, :hit, :time, :flair)"
    [ ":id" := unPostID (postId post),
      ":url" := postUrl post,
      ":title" := postTitle post,
      ":body" := postBody post,
      ":submitter" := postAuthor post,
      ":hit" := (if hit then 1 else 0 :: Integer),
      ":time" := formatTime defaultTimeLocale "%F %T" (postCreatedTime post),
      ":flair" := fromMaybe "" (postFlairText post)
    ]
  n <- fromOnly . head <$> (query_ sql "SELECT COUNT(*) FROM posts;" :: IO [Only Int])
  close sql
  printf "Added post to database (%d total posts)\n" n

getLatestHits :: Int -> Connection -> IO [(Text, Text, Text, Text, Text, Text)]
getLatestHits n conn =
  query_ conn (Query . T.pack $ (printf "SELECT id, url, title, submitter, time, flair FROM posts WHERE hit = 1 ORDER BY time DESC LIMIT %d" n :: String))

getLatestNonHits :: Int -> Connection -> IO [(Text, Text, Text, Text, Text, Text)]
getLatestNonHits n conn =
  query_ conn (Query . T.pack $ (printf "SELECT id, url, title, submitter, time, flair FROM posts WHERE hit = 0 ORDER BY time DESC LIMIT %d" n :: String))

getTotalRows :: Connection -> IO Int
getTotalRows conn =
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM posts WHERE hit = 1 or hit = 0;"

getTotalHits :: Connection -> IO Int
getTotalHits conn =
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM posts WHERE hit = 1;"

getTotalNumberLabelled :: Connection -> IO Int
getTotalNumberLabelled conn =
  fromOnly . head
    <$> query_ conn "SELECT COUNT(DISTINCT id) FROM votes;"

getNumberLabelledBy :: Text -> Connection -> IO Int
getNumberLabelledBy username conn =
  fromOnly . head
    <$> query_ conn (Query . T.pack $ printf "SELECT COUNT(*) FROM votes WHERE username = '%s';" username)

-- | Returns post ID, URL, title, body, submitter, time, flair
getNextUnlabelledPost :: Connection -> IO (Maybe (Text, Text, Text, Text, Text, Text, Text))
getNextUnlabelledPost conn = do
  unlabelled <-
    query_
      conn
      "SELECT id, url, title, body, submitter, time, flair FROM posts WHERE id NOT IN (SELECT id FROM votes) AND truth IS NULL ORDER BY RANDOM() LIMIT 1"
  case unlabelled of
    [] -> pure Nothing
    (p : _) -> pure (Just p)

-- | Record a user's vote on a post
addVote :: Text -> Text -> Int -> Connection -> IO ()
addVote postId voter vote conn = do
  executeNamed
    conn
    "INSERT INTO votes (id, username, vote) VALUES (:id, :username, :vote)"
    [ ":id" := postId,
      ":username" := voter,
      ":vote" := vote
    ]

getNumVotes :: Connection -> IO Int
getNumVotes conn =
  fromOnly . head
    <$> query_ conn "SELECT COUNT(*) FROM votes;"

-- | Get all votes by a given user. Returned in descending order of time, i.e.
-- most recent votes first.
getAllVotesBy :: Text -> Connection -> IO [(Text, Text, Text, Text, Int)]
getAllVotesBy username conn =
  queryNamed
    conn
    "SELECT votes.id, posts.title, posts.url, posts.submitter, votes.vote FROM posts INNER JOIN votes ON posts.id=votes.id WHERE votes.username = :username ORDER BY votes.n DESC;"
    [":username" := username]

-- | Set up the database from scratch. Do not use this unless you want to start
-- all over again...!
_populateDB :: IO ()
_populateDB = do
  sql <- getDataFileName (dbFileName config) >>= open
  execute_ sql "DROP TABLE posts"
  execute_
    sql
    "CREATE TABLE IF NOT EXISTS posts\
    \ (id TEXT NOT NULL PRIMARY KEY,\
    \ url TEXT NOT NULL,\
    \ title TEXT NOT NULL,\
    \ body TEXT NOT NULL, \
    \ submitter TEXT NOT NULL,\
    \ time TEXT NOT NULL,\
    \ flair TEXT NOT NULL, \
    \ hit INTEGER,\
    \ truth INTEGER);"
  ownerUsername <- getEnvAsText "REDDIT_USERNAME"
  ownerPassword <- getEnvAsText "REDDIT_PASSWORD"
  ownerClientId <- getEnvAsText "REDDIT_FE_ID"
  ownerClientSecret <- getEnvAsText "REDDIT_FE_SECRET"
  let creds = OwnerCredentials {..}
  env <- authenticate creds (userAgent config)

  runRedditT' env $ do
    last1000Posts <- subredditPosts 1000 "pokemontrades" New
    forM_ last1000Posts $ \post -> liftIO $ do
      executeNamed
        sql
        "INSERT INTO posts (id, url, title, body, submitter, time, flair) \
        \ VALUES (:id, :url, :title, :body, :submitter, :time, :flair)"
        [ ":id" := unPostID (postId post),
          ":url" := postUrl post,
          ":title" := postTitle post,
          ":body" := postBody post,
          ":submitter" := postAuthor post,
          ":time" := formatTime defaultTimeLocale "%F %T" (postCreatedTime post),
          ":flair" := fromMaybe "" (postFlairText post)
        ]

  close sql
