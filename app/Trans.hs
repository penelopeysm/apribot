-- | Monad transformer stack used for Apribot. Also re-exports Config and some
-- useful functions from mtl and transformers.
--
-- (Trans rights are human rights)
module Trans
  ( App (..),
    Config (..),
    runApp,
    runAppWith,
    atomically,
    atomicallyWith,
    Control.Monad.IO.Class.liftIO,
    Control.Monad.Reader.ask,
    Control.Monad.Reader.asks,
    Control.Monad.Reader.ReaderT (..),
    Control.Monad.forever,
    Control.Monad.void,
    Control.Monad.when,
    Control.Monad.Reader.lift,
  )
where

import Config
import Control.Concurrent (MVar, withMVar)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT, ask, asks, lift, runReaderT)

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
atomicallyWith :: (MonadIO m) => MVar () -> IO () -> m ()
atomicallyWith lock action = do
  liftIO $ withMVar lock $ const action

-- | Run an IO action atomically, using the lock in the config.
atomically :: MonadIO m => IO () -> App m ()
atomically action = do
  lock <- asks cfgLock
  liftIO $ atomicallyWith lock action
