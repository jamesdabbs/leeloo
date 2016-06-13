 module Bots
  ( demo
  , buildSlackBot
  ) where

import Base
import Bot

import Plugins.Base
import qualified Plugins.Panic as P

import qualified Adapters.CLI as CLI
import qualified Adapters.Slack as Slack

demo :: L ()
demo = do
  b:_ <- savedBots
  runBot $ buildBot CLI.adapter defaultPlugins b

defaultPlugins :: MonadIO m => [Adapter m -> Plugin m]
defaultPlugins = [echo, help, P.check, P.record, P.export]

buildSlackBot :: Entity Bot -> BotSpec L
buildSlackBot = buildBot Slack.adapter defaultPlugins
