{-# LANGUAGE FlexibleContexts #-}
module Plugins.Panic
  ( panic
  ) where

import Base
import App (AppError)
import Bot (redis)
import Plugin hiding (Adapter(..))
import Plugins.Base

import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Database.Redis.Namespace as R

panic :: (MonadError AppError m, BotM m) => Plugin m
panic = Plugin "panic" [check, record, export]

check :: (MonadError AppError m, BotM m) => Handler m
check = mkHandler "Start a panic score poll" True checkP [] $ \mroom -> do
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


record :: BotM m => Handler m
record = mkHandler "Record reported panic scores" False recordP [] $ \n -> do
  user <- getSender
  respondToPoll user n
  sendToUser user $ T.pack $ show n <> ", got it"

recordP :: Parser Int
recordP = do
  d <- satisfy $ \c -> c >= '1' && c <= '6'
  return $ read [d]


export :: BotM m => Handler m
export = mkHandler "Export panic scores" True (string "export panic") [] $ \_ ->
  error "export panic"


getRoom :: Monad m => Maybe Text -> H m (Maybe Room)
getRoom mname = case mname of
  Just name -> getRoomByName name
  _         -> Just . messageRoom <$> getMessage

startPoll :: (MonadError AppError m, BotM m) => Room -> H m ()
startPoll source = do
  members <- getRoomMembers source
  forM_ members $ \u -> sendToUser u "Hey, how are you doing today (on a scale of 1-6)?"

respondToPoll :: BotM m => User -> Int -> H m ()
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
