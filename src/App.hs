{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module App
  ( AppConf(..)
  , AppError(..)
  , AppUser(..)
  , AppUserToken
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
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Database.Redis              (Connection, connect, defaultConnectInfo)
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
              deriving Show

data BotStatus = BotStatus
  { bsBot    :: !Bot
  , bsStatus :: !(Maybe WorkerStatus)
  }

newtype L' m a = L'
  { unL' :: ExceptT AppError (ReaderT AppConf m) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConf, MonadError AppError)

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
