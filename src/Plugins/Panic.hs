{-# LANGUAGE FlexibleContexts #-}
module Plugins.Panic
  ( check
  , record
  , export
  ) where

import Base
import Plugin hiding (Adapter(..))
import Plugins.Base

import Data.Attoparsec.Text
import qualified Data.Text as T

import Database.Redis.Namespace


check :: BotM m => Plugin m
check = mkPlugin "panic.check" True checkP [] $ \mroom -> do
  reply "I don't know, but I'll ask them"
  getRoom mroom >>= \case
    Just roomId -> startPoll roomId
    Nothing -> reply "Sorry ... I couldn't figure out what room you meant"

checkP :: Parser (Maybe Text)
checkP = do
  string "how"
  manyTill anyChar (string "every" *> word)
  whitespace
  optional (string "in " *> word)


record :: BotM m => Plugin m
record = mkPlugin "panic.record" False recordP [] $ \n -> do
  user <- messageUser <$> message
  respondToPoll user n
  sendToUser user $ T.pack $ show n <> ", got it"

recordP :: Parser Int
recordP = do
  d <- satisfy $ \c -> c >= '1' && c <= '6'
  return $ read [d]


export :: BotM m => Plugin m
export = mkPlugin "panic.export" True (string "export panic") [] $ \_ ->
  error "export panic"


getRoom :: Monad m => Maybe Text -> Handler m (Maybe Room)
getRoom mname = case mname of
  Just name -> getRoomByName name
  _         -> Just . messageRoom <$> message

startPoll :: BotM m => Room -> Handler m ()
startPoll source = do
  redis $ do
    set "foo" "1"
    set "bar" "2"
  members <- getRoomMembers source
  forM_ members $ \u -> sendToUser u "Hey, how are you doing today (on a scale of 1-6)?"

respondToPoll :: BotM m => User -> Int -> Handler m ()
respondToPoll user n = do
  poll <- activePollFor user
  when (n > 4) $
    sendToUser (pollPoster poll) $ "FYI, " <> (T.pack $ show user) <> " is at a " <> (T.pack $ show n)
  recordPollResponse poll user n

data Poll = Poll
  { pollPoster :: User
  }

activePollFor :: User -> m Poll
activePollFor = error "activePollFor"

recordPollResponse :: Poll -> User -> Int -> m ()
recordPollResponse = error "recordPollResponse"
