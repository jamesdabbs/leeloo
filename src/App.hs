{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  ) where

import Base
import           Control.Monad.Logger        (MonadLogger(..))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BSC
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (pack)
import           Database.Redis.Namespace    (Connection, ConnectInfo(..), PortID(PortNumber)
                                             , connect, defaultConnectInfo)
import qualified Rollbar
import           System.Environment          (getEnv, lookupEnv)

import Replicant

import Replicant.Adapters.Slack.Types (Credentials(..))
import Logging                        (Logger, newLogger)

data AppConf = AppConf
  { appEnv              :: AppEnv
  , supervisor          :: Supervisor L BotId
  , redisConn           :: Connection
  , redisNS             :: Text
  , logger              :: Logger
  , slackAppCredentials :: Credentials
  , rollbarSettings     :: Maybe Rollbar.Settings
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

data AppEnv = Dev | Test | Prod deriving (Show, Read)

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
  redisNamespace = \case
                     Test -> "leeloo.test"
                     _    -> "leeloo"
                   <$> asks appEnv
  redisError _   = throwError RedisError

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runReplicantT

env :: String -> IO Text
env key = pack <$> getEnv key

getMode :: IO AppEnv
getMode = maybe Dev read <$> lookupEnv "LEELOO_ENV"

getSlackCredentials :: IO Credentials
getSlackCredentials = Credentials
  <$> env "SLACK_CLIENT_ID"
  <*> env "SLACK_CLIENT_SECRET"

getRollbarSettings :: AppEnv -> IO (Maybe Rollbar.Settings)
getRollbarSettings Prod = do
  tok  <- env "ROLLBAR_ACCESS_TOKEN"
  name <- fromMaybe "leelo.dev" <$> lookupEnv "HOSTNAME"
  return $ Just Rollbar.Settings
    { Rollbar.environment = Rollbar.Environment "Production"
    , Rollbar.token       = Rollbar.ApiToken tok
    , Rollbar.hostName    = name
    }
getRollbarSettings _ = return Nothing

getRedisConnection :: AppEnv -> IO Connection
getRedisConnection Prod = do -- TODO: make this conditional on presence of Redis ENV vars only
  host  <- getEnv "REDIS_HOST"
  -- TODO: what's going wrong with this read instance?
  -- port  <- read <$> getEnv "REDIS_PORT" -- :: IO PortNumber
  pool  <- maybe 10 read <$> lookupEnv "REDIS_MAX_CONNECTIONS"
  auth  <- lookupEnv "REDIS_AUTH"
  let info = defaultConnectInfo { connectHost           = host
                                , connectPort           = PortNumber 10500
                                , connectMaxConnections = pool
                                , connectAuth           = BSC.pack <$> auth
                                }
  print info
  connect info
getRedisConnection _ = connect defaultConnectInfo

mkConf :: IO AppConf
mkConf = do
  mode <- getMode
  AppConf
    <$> pure mode
    <*> newSupervisor
    <*> getRedisConnection mode
    <*> pure "leeloo"
    <*> newLogger
    <*> getSlackCredentials
    <*> getRollbarSettings mode
