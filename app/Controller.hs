{-# LANGUAGE FlexibleInstances #-}
module Controller
  ( botIndex
  , botCreate
  , botStart
  , botStop
  ) where

import Base

import Data.Aeson

import App          (L, bots)
import Bot          (getBot, runBot, saveBot)
import Bots         (buildSlackBot)
import Bot.Registry (BotStatus(..), getStatuses)

import qualified Adapters.Slack.Api as S (getBotInfo)

instance FromJSON BotInfo where
  parseJSON = withObject "bot_info" $ \v -> do
    botInfoToken <- v .: "token"
    botInfoIcon  <- v .: "icon"
    return BotInfo{..}

instance ToJSON BotStatus where
  toJSON BotStatus{..} = object
    [ "bot"    .= botSpec
    , "thread" .= thread
    ]
    where
      thread = drop 9 $ show botThreadId

botIndex :: L [BotStatus]
botIndex = asks bots >>= getStatuses

botCreate :: BotInfo -> L ()
botCreate info = do
  record <- S.getBotInfo info
  saveBot record
  runBot $ buildSlackBot record

botStart :: BotId -> L ()
botStart _id = do
  record <- getBot _id
  $logInfo $ "Starting bot: " <> botName record
  runBot $ buildSlackBot record

botStop :: BotId -> L ()
botStop = error "botStop"
