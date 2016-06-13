module Bot.Logic
  ( botDirectives
  ) where

import Base
import Plugins.Base
import qualified Plugins.Panic as P

import qualified Data.List as L

botDirectives :: MonadIO m => Adapter m -> Bot -> Message -> m ()
botDirectives a bot msg = do
  let plugins = map ($ a) [echo, help, P.check, P.record, P.export]

  let applicable = L.filter (\p -> pluginApplies p bot msg) plugins

  -- TODO:
  -- * ensure that plugins run in isolation and crash safely
  -- * enforce only one match?
  unless (null applicable) $ do
    let names = L.map pluginName applicable
    liftIO . putStrLn $ show msg ++ " matches plugins " ++ show names
    forM_ applicable $ \p -> runPlugin p bot msg
