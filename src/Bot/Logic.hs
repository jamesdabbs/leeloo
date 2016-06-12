module Bot.Logic
  ( botDirectives
  ) where

import Base
import Model (Bot(..))
import Plugins.Base
import Plugins.Panic

botDirectives :: MonadIO m => Adapter m -> Bot -> Message -> m ()
botDirectives a bot msg = do
  -- TODO: need to check / run these independently
  -- (so that one failing one doesn't stop the others from running)
  echo        a bot msg
  help        a bot msg
  checkPanic  a bot msg
  exportPanic a bot msg
