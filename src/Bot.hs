module Bot
  ( bootSaved
  , botDirectives
  , buildBot
  , getBot
  , getStatuses
  , newBotRegistry
  , runBot
  , saveBot
  , savedBots
  ) where

import Base
import Model
import Servant (throwError)

import Bot.Registry (getStatuses, newBotRegistry)

import qualified Data.List as L
import Database.Persist

savedBots :: L [Entity Bot]
savedBots = runDB $ selectList [] []

saveBot :: Bot -> L (Entity Bot)
saveBot b = runDB $ upsert b []

getBot :: BotId -> L (Entity Bot)
getBot _id = runDB (get _id) >>= \case
  Just r  -> return $ Entity _id r
  Nothing -> throwError NotFound

runBot :: Monad m => BotSpec m -> m ()
runBot spec@BotSpec{..} = bootBot botAdapter spec

botDirectives :: MonadIO m => BotSpec m -> Message -> m ()
botDirectives BotSpec{..} msg = do
  let (Entity _ bot) = botRecord
  let applicable = L.filter (\p -> pluginApplies p bot msg) botPlugins

  -- TODO:
  -- * ensure that plugins run in isolation and crash safely
  -- * enforce only one match?
  unless (null applicable) $ do
    let names = L.map pluginName applicable
    liftIO . putStrLn $ show msg ++ " matches plugins " ++ show names
    forM_ applicable $ \p -> runPlugin p bot msg

buildBot :: Adapter m -> [Adapter m -> Plugin m] -> Entity Bot -> BotSpec m
buildBot botAdapter plugs botRecord =
  let botPlugins = map ($ botAdapter) plugs
  in BotSpec{..}

bootSaved :: Adapter L -> L ()
bootSaved adapter = savedBots >>= mapM_ buildAndStart
  where
    defaultPlugins = error "defaultPlugins"
    startBotSpec b = bootBot (botAdapter b) b
    buildAndStart = startBotSpec . buildBot adapter defaultPlugins
