{-# LANGUAGE FlexibleContexts #-}
module Bots
 ( demo
 , buildSlackBot
 , mkConf
 ) where

import Base
import App
import Bot
import Plugin

import Plugins.Base
import qualified Plugins.Panic as P
import qualified Plugins.Score as P

import qualified Adapters.CLI as CLI
import qualified Adapters.Slack as Slack

demo :: IO ()
demo = do
  conf <- mkConf
  result <- runL conf $ do
    b <- head <$> savedBots
    runBot $ buildBot CLI.adapter defaultPlugins b
  either (error . show) return result

defaultPlugins :: [Plugin L]
defaultPlugins = [echo, help, P.check, P.record, P.export, P.score, P.scoreUp, P.scoreDown]

buildSlackBot :: Bot -> BotSpec L
buildSlackBot = buildBot Slack.adapter defaultPlugins
