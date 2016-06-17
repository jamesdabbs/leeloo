{-# LANGUAGE FlexibleInstances #-}
module Bot.Registry
  ( BotRegistry
  , BotStatus(..)
  , addBot
  , getStatuses
  , newBotRegistry
  ) where

import Base

import           Data.IORef           (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map             as Map

type BotRegistry = IORef (Map.Map BotId BotStatus)

data BotStatus = BotStatus
  { botSpec     :: !Bot
  , botThreadId :: !ThreadId
  -- TODO: other metadata, last checkin, list of plugins, &c.
  }

newBotRegistry :: MonadIO m => m BotRegistry
newBotRegistry = liftIO $ newIORef Map.empty

addBot :: MonadIO m => BotRegistry -> Bot -> ThreadId -> m ()
addBot registry bot thread = liftIO . atomicModifyIORef' registry $ \m -> (update m, ())
  where
    update    = Map.insert (botId bot) botStatus
    botStatus = BotStatus { botSpec = bot, botThreadId = thread }

getStatuses :: MonadIO m => BotRegistry -> m [BotStatus]
getStatuses registry = liftIO $ Map.elems <$> readIORef registry
