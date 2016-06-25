{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- We'll be defining a lot of ToJSON instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Controller
  ( botIndex
  , botStart
  , botStop
  , oauthCallback
  , pluginIndex
  ) where

import Base

import Data.Aeson

import App
import Bot.Supervisor (WorkerState(..), WorkerStatus(..))
import Bots
import Plugin

import qualified Adapters.Slack.Api as S

import qualified Data.Text as T

instance FromJSON BotInfo where
  parseJSON = withObject "bot_info" $ \v -> do
    botInfoToken <- v .: "token"
    botInfoIcon  <- v .: "icon"
    return BotInfo{..}

instance ToJSON ThreadId where
  toJSON t = Number . read . drop 9 $ show t

instance ToJSON WorkerState where
  toJSON WorkerRunning = "running"
  toJSON WorkerCrashed = "crashed"
  toJSON WorkerDone    = "done"

instance ToJSON WorkerStatus where
  toJSON WorkerStatus{..} = object
    [ "thread" .= wsThread
    , "error"  .= case wsError of
        Just err -> Just $ show err
        Nothing  -> Nothing
    , "state"  .= wsState
    ]

instance ToJSON BotStatus where
  toJSON BotStatus{..} = object
    [ "bot"    .= bsBot
    , "status" .= bsStatus
    ]

withUser :: Maybe AuthToken -> (AppUser -> L a) -> L a
withUser mtoken h = case mtoken of
  Nothing     -> throwError NotAuthenticated
  Just token  -> userFromToken token >>= \case
    Nothing   -> throwError NotAuthenticated
    Just user -> h user

botIndex :: Maybe AuthToken -> L [BotStatus]
botIndex tok = withUser tok $ \user ->
  savedBots user >>= getStatuses

botStart :: Maybe AuthToken -> BotId -> L ()
botStart tok _id = withUser tok $ \user -> do
  bot <- getBot user _id
  startBot $ buildSlackBot bot

botStop :: Maybe AuthToken -> BotId -> L ()
botStop tok _id = withUser tok $ \user -> do
  bot <- getBot user _id
  stopBot $ botId bot

oauthCallback :: Maybe Text -> L ()
oauthCallback mcode = case mcode of
  Nothing   -> redirectTo "/" -- TODO
  Just code -> do
    creds <- asks slackAppCredentials
    (u,b) <- S.oauth creds code

    (user, token) <- registerUser u
    bot <- registerBot user b
    welcomeUser bot user

    redirectTo $ "/todo#token=" <> decodeUtf8 token

redirectTo :: Text -> L () -- TODO: I don't love that this is an "error" an not well-reflected in the types
redirectTo url = throwError $ Redirect url

instance ToJSON PluginData where
  toJSON PluginData{..} = object
    [ "name"     .= pdName
    , "examples" .= (object $ map examplePair pdExamples)
    ]
    where
      examplePair Example{..} = exampleText .= exampleDescription

pluginIndex :: Maybe AuthToken -> L [PluginData]
pluginIndex tok = withUser tok $ \_ ->
  return $ map toPluginData defaultPlugins
  where
    toPluginData p@Plugin{..} = PluginData pluginName $ pluginExamples p
