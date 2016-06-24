module Adapters.Slack.Types
  ( Channel(..)
  , ChannelId
  , Credentials(..)
  , Event(..)
  , Message(..)
  , Token
  , User(..)
  , UserId
  ) where

import           Data.ByteString.Lazy        as LBS
import           Data.Text                   (Text)

type ChannelId = Text
type UserId    = Text
type Token     = Text

data Message = Message
  { messageBody    :: !Text
  , messageChannel :: !ChannelId
  , messageUser    :: !(Maybe Text)
  } deriving Show

data Channel = Channel
  { channelId   :: !ChannelId
  , channelName :: !Text
  } deriving Show

data User = User
  { userId   :: !UserId
  , userName :: !Text
  }

data Event = MessageEvent Message
           | MessageResponse
           | MessageError
           | UnknownEvent Text LBS.ByteString
           deriving Show

data Credentials = Credentials
  { appClientId     :: !Text
  , appClientSecret :: !Text
  } deriving (Show, Eq)
