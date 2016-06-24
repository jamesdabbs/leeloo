{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- We'll be defining a lot of ToJSON instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Controller
  ( botIndex
  , botCreate
  , botStart
  , botStop
  , oauthCallback
  , pluginIndex
  ) where

import Base

import Data.Aeson

import App
import Bot
import Bots         (buildSlackBot, defaultPlugins)
import Bot.Registry (BotStatus(..), getStatuses)
import Plugin

import qualified Adapters.Slack.Api as S

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
  runBot $ buildSlackBot record

botStop :: BotId -> L ()
botStop = error "botStop"

oauthCallback :: Maybe Text -> L ()
oauthCallback mcode = case mcode of
  Nothing   -> redirectTo "/" -- TODO
  Just code -> do
    creds <- asks slackAppCredentials
    (u,b) <- S.oauth creds code
    user  <- createUserAccount u
    bot   <- registerBot user b
    welcomeUser bot user

    token <- createUserToken user
    redirectTo $ "/todo#token=" <> token

redirectTo :: Text -> L () -- TODO: I don't love that this is an "error" an not well-reflected in the types
redirectTo url = throwError $ Redirect url

createUserAccount :: AppUserToken -> L AppUser
createUserAccount token = S.getBotInfo bot >>= saveAppUser . botToAppUser
  where bot = BotInfo token ""

registerBot :: AppUser -> BotToken -> L Bot
registerBot user token = do
  bot <- S.getBotInfo $ BotInfo token "pig"
  saveBot bot
  -- TODO: record bot owner / user
  runBot $ buildSlackBot bot
  return bot

welcomeUser :: Bot -> AppUser -> L ()
welcomeUser bot AppUser{..} = do
  S.sendMessage bot channel $ "Hi, " <> appUserName <> ". Thanks for the invite!"
  S.sendMessage bot channel $ "To get started and see what I can do, try saying `@" <> botName bot <> ": help` in a channel"
  where
    channel = "@" <> appUserName

createUserToken :: AppUser -> L Text
createUserToken AppUser{..} = do
  liftIO $ putStrLn "TODO: generate user token"
  return $ appUserId <> " (TODO: something more secure)"

instance ToJSON PluginData where
  toJSON PluginData{..} = object
    [ "name"     .= pdName
    , "examples" .= (object $ map examplePair pdExamples)
    ]
    where
      examplePair Example{..} = exampleText .= exampleDescription

pluginIndex :: L [PluginData]
pluginIndex = return $ map toPluginData defaultPlugins
  where
    toPluginData p@Plugin{..} = PluginData pluginName $ pluginExamples p
