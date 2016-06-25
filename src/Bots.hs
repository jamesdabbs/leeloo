{-# LANGUAGE FlexibleContexts #-}
module Bots
 ( buildSlackBot
 , defaultPlugins
 , getStatuses
 , mkConf
 , startBot
 , stopBot
 , startCli
 , startSavedBots
 ) where

import Base
import App
import Bot
import Bot.Supervisor (WorkerStatus(..), halt, monitor, status)
import qualified Logging as Log
import Plugin

import Plugins.Divide
import Plugins.Echo
import Plugins.Help
import Plugins.Panic
import Plugins.Score

import qualified Adapters.CLI as CLI
import qualified Adapters.Slack as Slack

import qualified Data.Map as M

import Control.Concurrent (threadDelay)

startCli :: AppConf -> IO ()
startCli conf = do
  result <- runL conf $ do
    let b = Bot { botId     = "B01"
                , botName   = "leeloo"
                , botIcon   = "^_^"
                , botToken  = "_token_"
                , botUserId = "B01"
                }
    startBot $ buildBot CLI.adapter defaultPlugins b
    CLI.wait
  either (error . show) return result

defaultPlugins :: [Plugin L]
defaultPlugins = [help, echo, score, panic, divide]


buildSlackBot :: Bot -> BotSpec L
buildSlackBot = buildBot Slack.adapter defaultPlugins

startSavedBots :: L ()
startSavedBots = savedBots >>= mapM_ (startBot . buildSlackBot)

startBot :: BotSpec L -> L ()
startBot spec@BotSpec{..} = do
  let Bot{..} = botRecord
  conf <- ask
  liftIO $ monitor (bots conf) botName botId (runL conf $ bootBot botAdapter spec)

stopBot :: BotId -> L ()
stopBot _id = do
  s <- supervisor
  liftIO $ halt s _id

getStatuses :: [Bot] -> L [BotStatus]
getStatuses bs = do
  stats <- supervisor >>= liftIO . status
  return $ map (\b@Bot{..} -> BotStatus b $ M.lookup botId stats) bs
