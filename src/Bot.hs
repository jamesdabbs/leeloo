module Bot
  ( boot
  , bootSaved
  , getBot
  , getStatuses
  , newBotRegistry
  , saveBot
  , savedBots
  ) where

import Base
import Model
import Servant (throwError)

import Bot.Boot     (boot)
import Bot.Registry (getStatuses, newBotRegistry)

import Database.Persist

savedBots :: L [Entity Bot]
savedBots = runDB $ selectList [] []

saveBot :: Bot -> L ()
saveBot b = runDB $ insertBy b >>= \case
  Left (Entity _id _) -> replace _id b
  Right _id -> return ()

getBot :: BotId -> L (Entity Bot)
getBot _id = do
  mfound <- runDB $ get _id
  case mfound of
    Just r  -> return $ Entity _id r
    Nothing -> throwError NotFound

bootSaved :: BotRegistry -> L ()
bootSaved registry = savedBots >>= mapM_ (liftIO . boot registry)
