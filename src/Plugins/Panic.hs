module Plugins.Panic
  ( checkPanic
  , exportPanic
  ) where

import Base
import Model
import Plugins.Base

import Data.Attoparsec.Text

checkPanic :: MonadIO m => Adapter m -> Bot -> Message -> m ()
checkPanic = onComment checkPanicP $ \a@Adapter{..} bot msg mroom -> do
  reply a bot msg "I don't know, but I'll ask them"
  getRoom a bot msg mroom >>= \case
    Just roomId -> startPoll a bot msg roomId
    Nothing -> reply a bot msg "Sorry ... I couldn't figure out what room you meant"

exportPanic :: MonadIO m => Adapter m -> Bot -> Message -> m ()
exportPanic = onComment (string "export panic") $ \a bot msg -> error "export panic"

checkPanicP :: Parser (Maybe Text)
checkPanicP = do
  string "how"
  manyTill anyChar (string "every" *> word)
  whitespace
  optional (string "in " *> word)

getRoom :: Monad m => Adapter m -> Bot -> Message -> Maybe Text -> m (Maybe Source)
getRoom a bot msg mname = case mname of
  Just name -> getRoomByName a bot name
  _         -> return . Just $ messageSource msg

startPoll :: MonadIO m => Adapter m -> Bot -> Message -> Source -> m ()
startPoll adapter bot msg source = do
  members <- getRoomMembers adapter bot source
  reply adapter bot msg "Should start poll"
