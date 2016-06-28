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

import           Control.Monad               (void)
import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader(..), asks)
import           Database.Redis.Namespace    (Connection, connect, defaultConnectInfo)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text, pack)
import qualified Rollbar
import           System.Environment          (getEnv)

import Replicant

import Replicant.Bot.Supervisor       (WorkerStatus(..))
import Replicant.Adapters.Slack.Types (Credentials(..))
import Logging                        (Logger, newLogger)

data AppConf = AppConf
  { supervisor          :: Supervisor L BotId
  , redisConn           :: Connection
  , redisNS             :: Text
  , logger              :: Logger
  , slackAppCredentials :: Credentials
  , rollbarSettings     :: Rollbar.Settings
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

type L = ReplicantT AppError AppConf IO

instance MonadLogger L where
  monadLoggerLog loc src lvl msg = do
    l <- asks logger
    liftIO . l $ toLogStr msg

instance Replicant AppError L where
  redisPool      = asks redisConn
  redisNamespace = return "leeloo"
  redisError _   = throwError RedisError

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runReplicantT

env :: String -> IO Text
env key = pack <$> getEnv key

getSlackCredentials :: IO Credentials
getSlackCredentials = Credentials
  <$> env "SLACK_CLIENT_ID"
  <*> env "SLACK_CLIENT_SECRET"

-- TODO: add Dev | Test | Prod environments (and use here and in Redis namespacing)
getRollbarSettings :: IO Rollbar.Settings
getRollbarSettings = do
  tok <- env "ROLLBAR_ACCESS_TOKEN"
  return Rollbar.Settings
    { Rollbar.environment = Rollbar.Environment "dev"
    , Rollbar.token       = Rollbar.ApiToken tok
    , Rollbar.hostName    = "leeloo.dev"
    }

getRedisConnection :: IO Connection
getRedisConnection = connect defaultConnectInfo -- TODO: check for env vars

mkConf :: IO AppConf
mkConf = AppConf
  <$> newSupervisor
  <*> getRedisConnection
  <*> pure "leeloo"
  <*> newLogger
  <*> getSlackCredentials
  <*> getRollbarSettings
