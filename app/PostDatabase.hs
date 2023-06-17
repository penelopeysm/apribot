-- | Database schema:
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
--    - agree      : Comma-separated list of people who think the post is
--                   interesting.
--    - disagree   : Comma-separated list of people who think the post is
--                   NOT interesting.
module PostDatabase (addToDb) where

import Config
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
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
    "INSERT INTO posts (id, url, title, body, submitter, hit, time, flair, agree, disagree) \
    \ VALUES (:id, :url, :title, :body, :submitter, :hit, :time, :flair, '', '')"
    [ ":id" := unPostID (postId post),
      ":url" := postUrl post,
      ":title" := postTitle post,
      ":body" := postBody post,
      ":submitter" := postAuthor post,
      ":hit" := (if hit then 1 else 0 :: Integer),
      ":time" := formatTime defaultTimeLocale "%F %T" (postCreatedTime post),
      ":flair" := fromMaybe "" (postFlairText post)
    ]
  n <- fromOnly . Prelude.head <$> (query_ sql "SELECT COUNT(*) FROM posts;" :: IO [Only Int])
  close sql
  printf "Added post to database (%d total posts)\n" n

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
    \ truth INTEGER, \
    \ agree TEXT NOT NULL, \
    \ disagree TEXT NOT NULL);"
  credsUsername <- getEnvAsText "REDDIT_USERNAME"
  credsPassword <- getEnvAsText "REDDIT_PASSWORD"
  credsClientId <- getEnvAsText "REDDIT_ID"
  credsClientSecret <- getEnvAsText "REDDIT_SECRET"
  let creds = Credentials {..}
  env <- authenticate creds (userAgent config)

  runRedditT' env $ do
    last1000Posts <- subredditPosts 1000 "pokemontrades" New
    forM_ last1000Posts $ \post -> liftIO $ do
      executeNamed
        sql
        "INSERT INTO posts (id, url, title, body, submitter, time, flair, agree, disagree) \
        \ VALUES (:id, :url, :title, :body, :submitter, :time, :flair, '', '')"
        [ ":id" := unPostID (postId post),
          ":url" := postUrl post,
          ":title" := postTitle post,
          ":body" := postBody post,
          ":submitter" := postAuthor post,
          ":time" := formatTime defaultTimeLocale "%F %T" (postCreatedTime post),
          ":flair" := fromMaybe "" (postFlairText post)
        ]

  close sql
