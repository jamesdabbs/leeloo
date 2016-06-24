module Types
  ( Bot(..)
  , BotM(..)
  , BotId
  , BotInfo(..)
  , BotName
  , BotToken
  , Message(..)
  , Namespace
  , Room(..)
  , RoomId
  , UserId
  , User(..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Data.ByteString        (ByteString)
import qualified Database.Redis         as Redis (Connection)

type BotId = Text
type RoomId = Text
type UserId = Text

type BotName = Text
type BotToken = Text

type Namespace = Text

data Bot = Bot
  { botId     :: BotId
  , botName   :: BotName
  , botIcon   :: Text
  , botToken  :: BotToken
  , botUserId :: UserId
  } deriving (Show, Eq)

data Room = Room
  { roomId   :: !RoomId
  , roomName :: !Text
  } deriving (Show, Eq)

data User = User
  { userId   :: !UserId
  , userName :: !Text
  } deriving (Show, Eq)

data Message = Message -- an _incoming_ message
  { messageRoom   :: !Room
  , messageUser   :: !User
  , messageText   :: !Text
  , messageDirect :: Bool
  -- TODO: better enforcement of non-nulls
  } deriving Show

data BotInfo = BotInfo
  { botInfoToken :: BotToken
  , botInfoIcon  :: Text
  }

class MonadIO m => BotM m where
  redisPool      :: m Redis.Connection
  redisNamespace :: m ByteString
