{-# LANGUAGE FlexibleContexts #-}
module Bots
 ( buildSlackBot
 , mkConf
 , startCli
 , startSavedBots
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

startCli :: AppConf -> IO ()
startCli conf = do
  putStrLn "Booting bot"
  result <- runL conf $ do
    -- b <- head <$> savedBots
    let b = Bot { botId     = "B01"
                , botName   = "leeloo"
                , botIcon   = "^_^"
                , botToken  = "_token_"
                , botUserId = "B01"
                }
    runBot $ buildBot CLI.adapter defaultPlugins b
  either (error . show) return result

defaultPlugins :: [Plugin L]
defaultPlugins = [echo, help, P.check, P.record, P.export, P.score, P.scoreUp, P.scoreDown]

buildSlackBot :: Bot -> BotSpec L
buildSlackBot = buildBot Slack.adapter defaultPlugins

startSavedBots :: L ()
startSavedBots = do
  bots <- savedBots
  mapM_ (bootBot Slack.adapter . buildSlackBot) bots
