{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( AppConf(..)
  , AppError(..)
  , BotRegistry
  , Channel
  , Message(..)
  , Event(..)
  , BotToken
  , L
  , runL
  , BotStatus(..)
  ) where

import           Control.Concurrent          (ThreadId)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader)
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

import Model

-- instance Ord Bot where
--   compare a b = compare (botToken a) (botToken b)

type BotRegistry = IORef (Map.Map BotId ThreadId)

type Channel = Text

data Message = Message
  { messageBody :: !Text
  , messageChannel :: !Channel
  , messageUser :: !(Maybe Text)
  } deriving Show
instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    messageBody      <- v .: "text"
    messageChannel   <- v .: "channel"
    messageUser      <- v .:? "user"
    return Message{..}


data Event = MessageEvent Message
           | MessageResponse
           | MessageError
           | UnknownEvent Text LBS.ByteString
           deriving Show

instance FromJSON Event where
  parseJSON = withObject "event" $ \v -> do
    typ <- v .:? "type"
    case typ of
      Just t  -> parseEvent t $ Object v
      Nothing -> do
        ok <- v .: "ok"
        if ok
          then return MessageResponse -- <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else return MessageError -- <$> v .: "reply_to" <*> v .: "error"

parseEvent :: Text -> Value -> Parser Event
parseEvent t = withObject "event" $ \v ->
  case t of
    "message" -> do
      m <- parseJSON $ Object v
      return $ MessageEvent m
    _ -> return $ UnknownEvent t (encode v)

type BotToken = Text

data AppConf = AppConf
  { db   :: ConnectionPool
  , bots :: BotRegistry
  }

data AppError = NotFound | Invalid deriving Show

newtype L' m a = L'
  { unL' :: ExceptT AppError (ReaderT AppConf m) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConf, MonadError AppError)

type L = L' IO

runL' :: Monad m => AppConf -> L' m a -> m (Either AppError a)
runL' conf m = runReaderT (runExceptT $ unL' m) conf

runL :: AppConf -> L a -> IO (Either AppError a)
runL = runL'

data BotStatus = BotStatus
  { botSpec     :: !(Entity Bot)
  , botThreadId :: !(Maybe ThreadId)
  }
