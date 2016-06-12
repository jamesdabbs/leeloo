{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types
  ( AppConf(..)
  , AppError(..)
  , BotInfo(..)
  , BotRegistry
  , BotStatus(..)
  , L
  , Logger
  , runL
  ) where

import           Control.Concurrent          (ThreadId)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.Aeson
import           Data.Aeson.Types            (Parser)
import           Data.ByteString.Lazy        as LBS
import           Data.IORef                  (IORef)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           Database.Persist            (Entity)
import           Database.Persist.Sql        (ConnectionPool)
import           System.Log.FastLogger       (FastLogger)

import Model
import qualified Types.Slack as Slack

data BotStatus = BotStatus
  { botSpec     :: !(Entity Bot)
  , botThreadId :: !(Maybe ThreadId)
  }

type BotRegistry = IORef (Map.Map BotId ThreadId)

type Logger = FastLogger

data AppConf = AppConf
  { db     :: ConnectionPool
  , bots   :: BotRegistry
  , logger :: Logger
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

runL' :: Monad m => AppConf -> L' m a -> m (Either AppError a)
runL' conf m = runReaderT (runExceptT $ unL' m) conf

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runL'

data BotInfo = BotInfo
  { botInfoToken :: Text
  , botInfoIcon  :: Text
  }
