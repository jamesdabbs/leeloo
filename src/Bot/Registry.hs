{-# LANGUAGE FlexibleInstances #-}
module Bot.Registry
  ( addBot
  , getStatuses
  , newBotRegistry
  ) where

import Base
import Model (Bot(..))

import           Data.IORef           (newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map             as Map

newBotRegistry :: IO BotRegistry
newBotRegistry = newIORef Map.empty

addBot :: BotRegistry -> Entity Bot -> ThreadId -> IO ()
addBot registry (Entity _id _) pid = atomicModifyIORef' registry $ \m -> (Map.insert _id pid m, ())

getStatuses :: BotRegistry -> [Entity Bot] -> IO [BotStatus]
getStatuses registry bots = do
  threads <- readIORef registry
  return $ map (fetch threads) bots
  where
    fetch ts bot = BotStatus
      { botSpec     = bot
      , botThreadId = Map.lookup (entityKey bot) ts
      }
