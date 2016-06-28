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
import Replicant.Bot.Supervisor (WorkerStatus(..))
import Bots

import Replicant.Types
import Replicant.Plugin
import qualified Replicant.Adapters.Slack.Api as S

import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           System.Environment          (lookupEnv)

instance FromJSON BotInfo where
  parseJSON = withObject "bot_info" $ \v -> do
    botInfoToken <- v .: "token"
    botInfoIcon  <- v .: "icon"
    return BotInfo{..}

instance ToJSON ThreadId where
  toJSON t = Number . read . drop 9 $ show t

instance ToJSON WorkerStatus where
  toJSON WorkerBooting          = object [ "status" .= ("booting" :: Text) ]
  toJSON (WorkerRunning thread) = object [ "status" .= ("running" :: Text) , "thread" .= thread ]
  toJSON (WorkerCrashed ex)     = object [ "status" .= ("crashed" :: Text) , "error" .= show ex ]
  toJSON WorkerDone             = object [ "status" .= ("done"    :: Text) ]

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

    domain <- fromMaybe "localhost:3000" <$> liftIO (lookupEnv "FRONTEND_DOMAIN")

    redirectTo $ T.pack domain <> "/#token=" <> decodeUtf8 token

redirectTo :: Text -> L () -- TODO: I don't love that this is an "error" an not well-reflected in the types
redirectTo url = throwError $ Redirect url

instance ToJSON PluginData where
  toJSON PluginData{..} = object
    [ "name"     .= pdName
    , "examples" .= object (map examplePair pdExamples)
    ]
    where
      examplePair Example{..} = exampleText .= exampleDescription

pluginIndex :: Maybe AuthToken -> L [PluginData]
pluginIndex tok = withUser tok $ \_ ->
  return $ map toPluginData defaultPlugins
  where
    toPluginData p@Plugin{..} = PluginData pluginName $ pluginExamples p
