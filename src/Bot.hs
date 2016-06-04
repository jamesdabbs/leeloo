{-# LANGUAGE LambdaCase #-}
module Bot
  ( boot
  , newBotRegistry
  , saveBot
  , savedBots
  ) where

import Base
import Model

import Bot.Boot     (boot)
import Bot.Registry (newBotRegistry)

import Database.Persist

savedBots :: L [Entity Bot]
savedBots = runDB $ selectList [] []

saveBot :: Bot -> L ()
saveBot b = runDB $ insertBy b >>= \case
  Left (Entity _id _) -> replace _id b
  Right _id -> return ()
