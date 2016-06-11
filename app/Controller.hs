{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Controller
  ( botIndex
  , botCreate
  , botStart
  , botStop
  ) where

import Base
import Model

import Data.Aeson
import Servant

import Bot (boot, getBot, getStatuses, savedBots)

import qualified Data.Text.Lazy as LT

instance ToJSON (Entity Bot) where
  toJSON (Entity _id Bot{..}) = object
    [ "id"    .= _id
    , "name"  .= botName
    , "token" .= botToken
    , "icon"  .= botIcon
    ]

instance ToJSON BotStatus where
  toJSON BotStatus{..} = object
    [ "bot"    .= botSpec
    , "thread" .= thread
    ]
    where
      thread = case botThreadId of
        Nothing -> Nothing
        Just t  -> Just . drop 9 $ show t

botIndex :: L [BotStatus]
botIndex = do
  specs   <- savedBots
  running <- asks bots
  liftIO $ getStatuses running specs

botCreate :: L ()
botCreate = error "botCreate"

botStart :: BotId -> L ()
botStart _id = do
  spec <- getBot _id
  $logInfo $ "Starting bot: " <> (LT.toStrict . botName $ entityVal spec)
  ask >>= flip boot spec

botStop :: BotId -> L ()
botStop = error "botStop"
