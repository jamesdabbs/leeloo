{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( bootSaved
  , botDirectives
  , buildBot
  , getBot
  , getStatuses
  , newBotRegistry
  , runBot
  , saveBot
  , savedBots
  ) where

import Base
import App
import Plugin
import Servant (throwError)

import Bot.Registry (getStatuses, newBotRegistry)

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List  as L
import           Database.Redis.Namespace (RedisNS, runRedisNS)
import qualified Database.Redis.Namespace as R

instance ToJSON Bot where
  toJSON Bot{..} = object
    [ "id"      .= botId
    , "name"    .= botName
    , "icon"    .= botIcon
    , "token"   .= botToken
    , "user_id" .= botUserId
    ]

instance FromJSON Bot where
  parseJSON = withObject "bot" $ \v -> do
    botId     <- v .: "id"
    botName   <- v .: "name"
    botIcon   <- v .: "icon"
    botToken  <- v .: "token"
    botUserId <- v .: "user_id"
    return Bot{..}

redis c = do
  conn <- asks redisConn
  liftIO $ runRedisNS conn "leeloo" c

savedBots :: L [Bot]
savedBots = error "savedBots" -- runDB $ selectList [] []

saveBot :: Bot -> L ()
saveBot b = void . redis $ R.set key val
  where
    key = encodeUtf8 $ "bot" <> botId b
    val = LBS.toStrict $ encode b

getBot :: BotId -> L Bot
getBot _id = error "getBot" -- redis $ R.get key
  where
    key = encodeUtf8 $ "bot" <> _id

runBot :: MonadIO m => BotSpec m -> m ()
runBot spec@BotSpec{..} = bootBot botAdapter spec

botDirectives :: MonadIO m => BotSpec m -> Message -> m ()
botDirectives BotSpec{..} msg = do
  let applicable = L.filter (\p -> pluginApplies p botRecord msg) botPlugins

  -- TODO:
  -- * ensure that plugins run in isolation and crash safely
  -- * enforce only one match?
  unless (null applicable) $ do
    let names = L.map pluginName applicable
    liftIO . putStrLn $ show msg ++ " matches plugins " ++ show names
    forM_ applicable $ \p -> runPlugin p botRecord msg

buildBot :: Adapter m -> [Plugin m] -> Bot -> BotSpec m
buildBot botAdapter botPlugins botRecord = BotSpec{..}

bootSaved :: Adapter L -> L ()
bootSaved adapter = savedBots >>= mapM_ buildAndStart
  where
    defaultPlugins = error "defaultPlugins"
    startBotSpec b = bootBot (botAdapter b) b
    buildAndStart = startBotSpec . buildBot adapter defaultPlugins
