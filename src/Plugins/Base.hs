{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Plugins.Base
  ( module Base
  -- re-exports
  , AppError(..)
  , Example(..)
  , Handler
  , H
  , Plugin(..)
  , botHandlers
  , mkHandler
  , redis
  -- locals
  , getBot
  , getMessage
  , getRoomByName
  , getRoomMembers
  , getSender
  , reply
  , sendToRoom
  , sendToUser
  , whitespace
  , word
  ) where

import           Base
import           App    (AppError(..))
import           Bot    (redis)
import           Plugin (Adapter, BotSpec(..), Example(..), H, Handler, Plugin(..), botHandlers, mkHandler)
import qualified Plugin as P

import           Control.Monad.Reader (lift)
import           Data.Attoparsec.Text
import qualified Data.Text as T

whitespace :: Parser ()
whitespace = void . many . satisfy $ inClass [' ', '\t', '\n']

word :: Parser Text
word = T.pack <$> many' letter

getMessage :: Monad m => H m Message
getMessage = asks P.handlerMessage

getBot :: Monad m => H m (BotSpec m)
getBot = asks P.handlerBot

getSender :: Monad m => H m User
getSender = messageUser <$> getMessage

reply :: Monad m => Text -> H m ()
reply text = do
  BotSpec{..} <- getBot
  Message{..} <- getMessage
  if messageDirect
    then sendToUser messageUser text
    else sendToRoom messageRoom text


-- Lifted versions of handler helper functions

sendToUser :: Monad m => User -> Text -> H m ()
sendToUser = liftH2 P.sendToUser

sendToRoom :: Monad m => Room -> Text -> H m ()
sendToRoom = liftH2 P.sendToRoom

getRoomByName :: Monad m => Text -> H m (Maybe Room)
getRoomByName = liftH P.getRoomByName

getRoomMembers :: Monad m => Room -> H m [User]
getRoomMembers = liftH P.getRoomMembers

liftH :: Monad m => (Adapter m -> Bot -> a -> m r) -> a -> H m r
liftH h a = do
  BotSpec{..} <- getBot
  lift $ h botAdapter botRecord a

liftH2 :: Monad m => (Adapter m -> Bot -> a -> b -> m r) -> a -> b -> H m r
liftH2 h a b = do
  BotSpec{..} <- getBot
  lift $ h botAdapter botRecord a b
