{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Monad transformer stack used for Apribot. Also re-exports Config and some
-- useful functions from mtl and transformers.
--
-- (Trans rights are human rights)
module Trans
  ( App (..),
    Config (..),
    Config.NotifyEvent (..),
    runApp,
    runAppWith,
    atomically,
    atomicallyWith,
    authenticateAsOwner,
    withAppPsqlConn,
    Control.Monad.IO.Class.liftIO,
    Control.Monad.Reader.ask,
    Control.Monad.Reader.asks,
    Control.Monad.Reader.local,
    Control.Monad.Reader.ReaderT (..),
    Control.Monad.forever,
    Control.Monad.void,
    Control.Monad.when,
    Control.Monad.Reader.lift,
  )
where

import Config
import Control.Concurrent (MVar, withMVar)
import Control.Exception (bracket)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT, ask, asks, lift, runReaderT, local)
import Database.PostgreSQL.Simple (Connection, close)
import Reddit (Credentials (..), RedditEnv, authenticate)

newtype App m a = App {unApp :: ReaderT Config m a}
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadTrans)

instance (MonadIO m) => MonadIO (App m) where
  liftIO = App . liftIO

runApp :: (MonadIO m) => App m a -> m a
runApp app = do
  cfg <- liftIO getConfig
  runReaderT (unApp app) cfg

runAppWith :: Config -> App m a -> m a
runAppWith cfg app = runReaderT (unApp app) cfg

-- Utilities for this monad

-- | Run an IO action atomically, using the given lock. Useful when you've
-- escaped from App into pure IO.
atomicallyWith :: (MonadIO m) => MVar () -> IO a -> m a
atomicallyWith lock action = do
  liftIO $ withMVar lock $ const action

-- | Run an IO action atomically, using the lock in the config.
atomically :: (MonadIO m) => IO a -> App m a
atomically action = do
  lock <- asks cfgLock
  liftIO $ atomicallyWith lock action

-- | Authenticate to Reddit with owner credentials
authenticateAsOwner :: (MonadIO m) => App m RedditEnv
authenticateAsOwner = do
  ownerUsername <- asks cfgRedditUsername
  ownerPassword <- asks cfgRedditPassword
  ownerClientId <- asks cfgRedditId
  ownerClientSecret <- asks cfgRedditSecret
  userAgent <- asks cfgUserAgent
  liftIO $ authenticate OwnerCredentials {..} userAgent

withAppPsqlConn :: (MonadIO m) => (Connection -> IO a) -> App m a
withAppPsqlConn action = do
  cfg <- ask
  liftIO $ bracket (runAppWith cfg $ liftIO $ cfgPsqlConn cfg) close action
