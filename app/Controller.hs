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

import Bot  (getBot, getStatuses, runBot, saveBot, savedBots)
import Bots (buildSlackBot)

import qualified Adapters.Slack as S
import qualified Adapters.Slack.Api as S (getBotInfo)

import qualified Data.Text.Lazy as LT

instance FromJSON BotInfo where
  parseJSON = withObject "bot_info" $ \v -> do
    botInfoToken <- v .: "token"
    botInfoIcon  <- v .: "icon"
    return BotInfo{..}

instance ToJSON (Entity Bot) where
  toJSON (Entity _id Bot{..}) = object
    [ "id"       .= _id
    , "slack_id" .= botUserId
    , "name"     .= botName
    , "token"    .= botToken
    , "icon"     .= botIcon
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

botCreate :: BotInfo -> L ()
botCreate info = do
  record <- S.getBotInfo info >>= saveBot
  runBot $ buildSlackBot record

botStart :: BotId -> L ()
botStart _id = do
  record <- getBot _id
  $logInfo $ "Starting bot: " <> (LT.toStrict . botName $ entityVal record)
  runBot $ buildSlackBot record

botStop :: BotId -> L ()
botStop = error "botStop"
