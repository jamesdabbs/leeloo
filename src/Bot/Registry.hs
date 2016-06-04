{-# LANGUAGE FlexibleInstances #-}
module Bot.Registry
  ( newBotRegistry
  , addBot
  ) where

import Base
import Model (Bot(..), BotId)

import           Data.IORef           (newIORef, modifyIORef')
import qualified Data.Map             as Map
import           Database.Persist     (Entity(..))

newBotRegistry :: IO (BotRegistry)
newBotRegistry = newIORef Map.empty

addBot :: BotRegistry -> Entity Bot -> ThreadId -> IO ()
addBot registry (Entity _id _) pid = modifyIORef' registry $ Map.insert _id pid
