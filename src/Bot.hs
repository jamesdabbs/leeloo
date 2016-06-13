module Bot
  ( bootSaved
  , getBot
  , getStatuses
  , newBotRegistry
  , saveBot
  , savedBots
  ) where

import Base
import Model
import Servant (throwError)
-- import qualified Adapters.Slack as Slack
import qualified Adapters.CLI as CLI

import Bot.Registry (getStatuses, newBotRegistry)

import Database.Persist

savedBots :: L [Entity Bot]
savedBots = runDB $ selectList [] []

saveBot :: Bot -> L (Entity Bot)
saveBot b = runDB $ upsert b []

getBot :: BotId -> L (Entity Bot)
getBot _id = runDB (get _id) >>= \case
  Just r  -> return $ Entity _id r
  Nothing -> throwError NotFound

bootSaved :: L ()
bootSaved = do
  let adapter = CLI.adapter
  b:_ <- savedBots
  bootBot adapter b
  -- savedBots >>= mapM_ (bootBot adapter)
