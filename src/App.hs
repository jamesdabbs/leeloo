{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module App
  ( AppConf(..)
  , AppError(..)
  , AppUser(..)
  , AppUserToken
  , AuthToken
  , BotStatus(..)
  , L
  , mkConf
  , runL
  , supervisor
  ) where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader(..), asks)
import           Control.Monad.Base
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Database.Redis              (Connection, connect, defaultConnectInfo)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text, pack)
import           System.Environment          (getEnv)

import Types
import Adapters.Slack.Types (Credentials(..))
import Bot.Supervisor       (Supervisor, WorkerStatus, newSupervisor)
import Logging              (Logger, newLogger)

data AppConf = AppConf
  { bots                :: Supervisor BotId
  , redisConn           :: Connection
  , redisNS             :: Text
  , logger              :: Logger
  , slackAppCredentials :: Credentials
  }

type AppUserToken = Text
data AppUser = AppUser
  { appUserId    :: Text
  , appUserName  :: Text
  , appUserToken :: AppUserToken
  } deriving (Show, Eq)

data AppError = NotFound
              | Invalid
              | Redirect Text
              | RedisError
              | NotAuthenticated
              deriving Show

type AuthToken = ByteString

data BotStatus = BotStatus
  { bsBot    :: !Bot
  , bsStatus :: !(Maybe WorkerStatus)
  }

newtype L' m a = L'
  { unL' :: ExceptT AppError (ReaderT AppConf m) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConf, MonadError AppError)

deriving instance MonadBase b m => MonadBase b (L' m)

instance MonadTrans L' where
  lift = L' . lift . lift

-- TODO: understand this more better
instance MonadBaseControl IO m => MonadBaseControl IO (L' m) where
  type StM (L' m) a = ComposeSt L' m a
  liftBaseWith      = defaultLiftBaseWith
  restoreM          = defaultRestoreM

instance MonadTransControl L' where
  type StT L' a = StT (ExceptT AppError) (StT (ReaderT AppConf) a)
  liftWith f = L' $ liftWith $ \run ->
                                liftWith $ \run' ->
                                            f (run' . run . unL')
  restoreT = L' . restoreT . restoreT

type L = L' IO

instance MonadLogger L where
  monadLoggerLog loc src lvl msg = do
    l <- asks logger
    liftIO . l $ toLogStr msg

instance BotM L where
  redisPool      = asks redisConn
  redisNamespace = return "leeloo"

runL' :: Monad m => AppConf -> L' m a -> m (Either AppError a)
runL' conf m = runReaderT (runExceptT $ unL' m) conf

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runL'

env :: String -> IO Text
env key = pack <$> getEnv key

getSlackCredentials :: IO Credentials
getSlackCredentials = Credentials
  <$> env "SLACK_CLIENT_ID"
  <*> env "SLACK_CLIENT_SECRET"

mkConf :: IO AppConf
mkConf = AppConf
  <$> newSupervisor
  <*> connect defaultConnectInfo
  <*> pure "leeloo"
  <*> newLogger
  <*> getSlackCredentials

supervisor :: L (Supervisor BotId)
supervisor = asks bots
