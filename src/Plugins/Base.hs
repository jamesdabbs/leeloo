{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Plugins.Base
  ( echo
  , getRoomByName
  , getRoomMembers
  , help
  , message
  , redis
  , reply
  , sendToUser
  , whitespace
  , word
  ) where

import           Base
import           Plugin hiding (Adapter(..))
import qualified Plugin as P

import           Control.Monad.Reader (lift)
import           Data.Attoparsec.Text
import qualified Data.Text as T
import           Database.Redis.Namespace

echo :: BotM m => Plugin m
echo = mkPlugin "echo" True ("echo " *> takeText) [] reply

help :: BotM m => Plugin m
help = mkPlugin "help" False (string "help") [] $ \_ ->
  reply "Should say something helpful here"

whitespace :: Parser ()
whitespace = void . many . satisfy $ inClass [' ', '\t', '\n']

word :: Parser Text
word = T.pack <$> many' letter

message :: Monad m => Handler m Message
message = asks handlerMessage

bot :: Monad m => Handler m (BotSpec m)
bot = asks handlerBot

reply :: Monad m => Text -> Handler m ()
reply text = do
  BotSpec{..} <- bot
  Message{..} <- message
  if messageDirect
    then sendToUser messageUser text
    else lift $ P.sendToRoom botAdapter botRecord messageRoom text

sendToUser :: Monad m => User -> Text -> Handler m ()
sendToUser user text = do
  BotSpec{..} <- bot
  lift $ P.sendToUser botAdapter botRecord user text

getRoomByName :: Monad m => Text -> Handler m (Maybe Room)
getRoomByName name = do
  BotSpec{..} <- bot
  lift $ P.getRoomByName botAdapter botRecord name

getRoomMembers :: Monad m => Room -> Handler m [User]
getRoomMembers room = do
  BotSpec{..} <- bot
  lift $ P.getRoomMembers botAdapter botRecord room

redis q = do
  conn <- asks handlerRedisConn
  ns   <- asks handlerNamespace
  res  <- liftIO $ runRedisNS conn ns q
  case res of
    Left  _ -> error "left redis error"
    Right r -> return r
