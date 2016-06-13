module Types.Slack
  ( Channel(..)
  , ChannelId
  , Event(..)
  , Message(..)
  , Token
  , UserId
  ) where

import           Data.Aeson
import           Data.Aeson.Types            (Parser)
import           Data.ByteString.Lazy        as LBS
import           Data.Text                   (Text)

type ChannelId = Text
type UserId    = Text
type Token     = Text

data Message = Message
  { messageBody    :: !Text
  , messageChannel :: !ChannelId
  , messageUser    :: !(Maybe UserId)
  } deriving Show

data Channel = Channel
  { channelId   :: !ChannelId
  , channelName :: !Text
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
