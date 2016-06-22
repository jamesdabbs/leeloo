{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module App
  ( AppConf(..)
  , AppError(..)
  , L
  , mkConf
  , runL
  ) where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader(..), asks, local)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Database.Redis              (Connection, connect, defaultConnectInfo)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)

import Types
import Plugin (BotM(..))
import Bot.Registry
import Logging (Logger, newLogger)

data AppConf = AppConf
  { bots      :: BotRegistry
  , redisConn :: Connection
  , redisNS   :: Text
  , logger    :: Logger
  }

data AppError = NotFound | Invalid deriving Show

newtype L' m a = L'
  { unL' :: ExceptT AppError (ReaderT AppConf m) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConf, MonadError AppError)

type L = L' IO

instance MonadLogger L where
  monadLoggerLog loc src lvl msg = do
    l <- asks logger
    liftIO . l $ toLogStr msg

instance BotM L where
  redisPool = asks redisConn

runL' :: Monad m => AppConf -> L' m a -> m (Either AppError a)
runL' conf m = runReaderT (runExceptT $ unL' m) conf

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runL'

mkConf :: IO AppConf
mkConf = AppConf
           <$> newBotRegistry
           <*> connect defaultConnectInfo
           <*> pure "leeloo"
           <*> newLogger
