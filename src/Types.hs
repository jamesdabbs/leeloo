{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types
  ( Adapter(..)
  , AppConf(..)
  , AppError(..)
  , Bot(..)
  , BotInfo(..)
  , BotRegistry
  , BotStatus(..)
  , L
  , Logger
  , Message(..)
  , PluginSpec(..)
  , Plugin -- N.B. not exposing constructor here
  , RoomId
  , Source(..)
  , UserId
  , User(..)
  , mkPlugin
  , runL
  ) where

import           Control.Concurrent          (ThreadId)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.Attoparsec.Text        (Parser, parseOnly)
import           Data.IORef                  (IORef)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           Database.Persist            (Entity)
import           Database.Persist.Sql        (ConnectionPool)
import           System.Log.FastLogger       (FastLogger)

import Model

type RoomId = Text
type UserId = Text

data Room = Room
  { roomId   :: !RoomId
  , roomName :: !Text
  }

data User = User
  { userId   :: !UserId
  , userName :: !Text
  } deriving Show

data Monad m => Adapter m = Adapter
  { bootBot        :: Entity Bot -> m ()
  , sendMessage    :: Bot -> Source -> Text -> m ()
  , parseCommand   :: Bot -> Message -> Maybe Text
  , getRoomByName  :: Bot -> Text -> m (Maybe Source)
  , getRoomMembers :: Bot -> Source -> m [User]
  }

data Example = Example
  { exampleTexts       :: [Text]
  , exampleDescription :: Text
  }

data Monad m => PluginSpec m a = PluginSpec
  { ppName     :: Text
  , ppExamples :: [Example]
  , ppParser   :: Parser a
  , ppHandler  :: a -> Bot -> Message -> m ()
  , ppCommand  :: Bool
  }

data Plugin m = Plugin
  { pName     :: Text
  , pExamples :: [Example]
  , pTest     :: Message -> Bool
  , pRun      :: Bot -> Message -> m ()
  , pCommand  :: Bool
  }

checkParse parser Message{..} =
  case parseOnly parser messageText of
    Right _ -> True
    Left  _ -> False

ppRun parser handler bot msg@Message{..} =
  case parseOnly parser messageText of
    Right r -> handler r bot msg
    Left  _ -> return ()

mkPlugin :: Monad m => PluginSpec m a -> Plugin m
mkPlugin PluginSpec{..} = Plugin
  { pName     = ppName
  , pExamples = ppExamples
  , pCommand  = ppCommand
  , pTest     = checkParse ppParser
  , pRun      = ppRun ppParser ppHandler
  }

-- Idea:
-- * enforce that plugins parse their examples
-- * (optional) enforce that other plugins _don't_ parse
data BotSpec m = BotSpec
  { botRecord  :: !(Entity Bot)
  , botAdapter :: !(Adapter m)
  -- TODO: we don't want to restrict the plugin type `a`,
  --   so this will be a list of names that are assumed unique.
  --   I'd like to verify this though, instead of stringly typing
  , botPlugins :: ![Text]
  }

data BotStatus = BotStatus
  { botSpec     :: !(Entity Bot)
  , botThreadId :: !(Maybe ThreadId)
  -- TODO: other metadata, last checkin, list of plugins, &c.
  }

data Source = SourceRoom Text | SourceUser User deriving Show

data Message = Message
  { messageSource :: !Source
  , messageText   :: !Text
  } deriving Show

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
