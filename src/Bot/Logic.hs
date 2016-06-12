module Bot.Logic
  ( botDirectives
  ) where

import Base
import Model (Bot(..))
import Plugins.Base
import qualified Plugins.Panic as P

botDirectives :: MonadIO m => Adapter m -> Bot -> Message -> m ()
botDirectives a bot msg = do
  -- TODO: need to check / run these independently
  -- (so that one failing one doesn't stop the others from running)
  echo     a bot msg
  help     a bot msg
  P.check  a bot msg
  P.record a bot msg
  P.export a bot msg
