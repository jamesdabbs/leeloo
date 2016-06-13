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
  , BotSpec(..)
  , BotStatus(..)
  , L
  , Logger
  , Message(..)
  , Plugin -- N.B. not exposing constructor here
  , Room(..)
  , RoomId
  , UserId
  , User(..)
  , mkPlugin
  , pluginApplies
  , pluginName
  , runL
  , runPlugin
  ) where

import           Control.Concurrent          (ThreadId)
import           Control.Monad               (MonadPlus, mzero, unless)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger(..), toLogStr)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.Attoparsec.Text        (Parser, parseOnly)
import           Data.IORef                  (IORef)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
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
  } deriving (Show, Eq)

data User = User
  { userId   :: !UserId
  , userName :: !Text
  } deriving (Show, Eq)

data Monad m => Adapter m = Adapter
  { bootBot        :: BotSpec m -> m ()
  , sendToRoom     :: Bot -> Room -> Text -> m ()
  , sendToUser     :: Bot -> User -> Text -> m ()
  , parseCommand   :: Bot -> Message -> Maybe Text
  , getRoomByName  :: Bot -> Text -> m (Maybe Room)
  , getRoomMembers :: Bot -> Room -> m [User]
  }

data Example = Example
  { exampleTexts       :: [Text]
  , exampleDescription :: Text
  }

data Monad m => Plugin m = Plugin
  { pName     :: Text
  , pExamples :: [Example]
  , pCommand  :: Bool
  , pAdapter  :: Adapter m
  -- The odd version of Bool is so we can have a consistent
  -- monadic interface to both these helpers
  -- We always `return ()` if the command check fails
  -- These aren't exposed, so it's not that bad, but ...
  -- TODO: make this more intuitive
  , pTest     :: Bot -> Message -> Maybe ()
  , pRun      :: Bot -> Message -> m ()
  }

checkParser :: Parser a -> Bot -> Message -> Maybe ()
checkParser parser bot Message{..} =
  case parseOnly parser messageText of
    Right _ -> Nothing
    Left  _ -> Just ()

runParser :: Monad m => Parser a -> (Bot -> Message -> a -> m ()) -> Bot -> Message -> m ()
runParser parser handler bot msg@Message{..} =
  case parseOnly parser messageText of
    Right r -> handler bot msg r
    Left  _ -> return ()

pluginApplies :: Monad m => Plugin m -> Bot -> Message -> Bool
pluginApplies p b m = isNothing $ pTest p b m

pluginName :: Monad m => Plugin m -> Text
pluginName = pName

runPlugin :: Monad m => Plugin m -> Bot -> Message -> m ()
runPlugin = pRun

mkPlugin :: Monad m
         => Adapter m
         -> Text
         -> [Example]
         -> Bool
         -> Parser a
         -> (Bot -> Message -> a -> m ())
         -> Plugin m
mkPlugin adapter name examples commandOnly parser handler = Plugin
  { pName     = name
  , pExamples = examples
  , pCommand  = commandOnly
  , pAdapter  = adapter
  , pTest     = withCommand $ checkParser parser
  , pRun      = withCommand $ runParser parser handler
  }
  where
    withCommand :: Monad m => (Bot -> Message -> m ()) -> Bot -> Message -> m ()
    withCommand f b m = case parseCommand adapter b m of
      Just text -> f b $ m { messageText = text }
      _ -> -- This didn't match the command format for the given adapter
        if commandOnly
          then return ()
          else f b m

-- Idea:
-- * enforce that plugins parse their examples
-- * (optional) enforce that other plugins _don't_ parse
data BotSpec m = BotSpec
  { botRecord  :: !(Entity Bot)
  , botAdapter :: !(Adapter m)
  , botPlugins :: ![Plugin m]
  }

data BotStatus = BotStatus
  { botSpec     :: !(Entity Bot)
  , botThreadId :: !(Maybe ThreadId)
  -- TODO: other metadata, last checkin, list of plugins, &c.
  }

data Message = Message -- an _incoming_ message
  { messageRoom   :: !Room
  , messageUser   :: !User
  , messageText   :: !Text
  , messageDirect :: Bool
  -- TODO: better enforcement of non-nulls
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
