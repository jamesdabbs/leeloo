module Plugins.Panic
  ( check
  , record
  , export
  ) where

import Base
import Model
import Plugins.Base

import Data.Attoparsec.Text
import qualified Data.Text as T

check :: MonadIO m => Adapter m -> Plugin m
check a = mkPlugin a "Check panic" [] True checkP $ \bot msg mroom -> do
  reply a bot msg "I don't know, but I'll ask them"
  getRoom a bot msg mroom >>= \case
    Just roomId -> startPoll a bot msg roomId
    Nothing -> reply a bot msg "Sorry ... I couldn't figure out what room you meant"

checkP :: Parser (Maybe Text)
checkP = do
  string "how"
  manyTill anyChar (string "every" *> word)
  whitespace
  optional (string "in " *> word)


record :: MonadIO m => Adapter m -> Plugin m
record a = mkPlugin a "Record panic levels" [] True recordP $ \bot msg@Message{..} n -> case messageSource of
  SourceUser user -> do
    respondToPoll a bot user n
    reply a bot msg $ T.pack $ show n <> ", got it"
  _ -> return () -- TODO: this is klunky

recordP :: Parser Int
recordP = do
  d <- satisfy $ \c -> c >= '1' && c <= '6'
  return $ read [d]


export :: MonadIO m => Adapter m -> Plugin m
export a = mkPlugin a "Export all panic scores" [] True (string "export panic") $ \bot msg _ ->
  error "export panic"


getRoom :: Monad m => Adapter m -> Bot -> Message -> Maybe Text -> m (Maybe Source)
getRoom a bot msg mname = case mname of
  Just name -> getRoomByName a bot name
  _         -> return . Just $ messageSource msg

startPoll :: MonadIO m => Adapter m -> Bot -> Message -> Source -> m ()
startPoll adapter bot msg source = do
  members <- getRoomMembers adapter bot source
  forM_ members $ \u ->
    sendMessage adapter bot (SourceUser u) "Hey, how are you doing today (on a scale of 1-6)?"

respondToPoll :: MonadIO m => Adapter m -> Bot -> User -> Int -> m ()
respondToPoll adapter bot user n = do
  poll <- activePollFor user
  when (n > 4) $
    sendMessage adapter bot (SourceUser $ pollPoster poll) $ "FYI, " <> (T.pack $ show user) <> " is at a " <> (T.pack $ show n)
  recordPollResponse poll user n

data Poll = Poll
  { pollPoster :: User
  }

activePollFor :: User -> m Poll
activePollFor = error "activePollFor"

recordPollResponse :: Poll -> User -> Int -> m ()
recordPollResponse = error "recordPollResponse"

