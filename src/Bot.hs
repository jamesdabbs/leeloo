{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( botDirectives
  , botToAppUser
  , buildBot
  , getBot
  , getStatuses
  , newBotRegistry
  , redis
  , runBot
  , saveAppUser
  , saveBot
  , savedBots
  ) where

import Base
import App
import Plugin
import qualified Logging as Log

import Bot.Registry (getStatuses, newBotRegistry)

import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as L
import qualified Database.Redis           as R (Redis, Reply)
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

instance ToJSON AppUser where
  toJSON AppUser{..} = object
    [ "id"      .= appUserId
    , "name"    .= appUserName
    , "token"   .= appUserToken
    ]

instance FromJSON AppUser where
  parseJSON = withObject "app user" $ \v -> do
    appUserId    <- v .: "id"
    appUserName  <- v .: "name"
    appUserToken <- v .: "token"
    return AppUser{..}

redis :: (MonadError AppError m, BotM m) => RedisNS R.Redis (Either R.Reply) a -> m a
redis q = do
  conn <- redisPool
  ns   <- redisNamespace
  res  <- liftIO $ runRedisNS conn ns q
  case res of
    Left  _ -> throwError RedisError
    Right r -> return r

savedBots :: L [Bot]
savedBots = do
  ids <- redis $ R.smembers "bots"
  mapM (getBot . decodeUtf8) ids -- TODO: mget?

saveBot :: Bot -> L ()
saveBot = void . save "bots" botId

saveAppUser :: AppUser -> L AppUser
saveAppUser = save "users" appUserId

botToAppUser :: Bot -> AppUser
botToAppUser Bot{..} =
  let
    appUserName  = botName
    appUserToken = botToken
    appUserId    = botId
  in AppUser{..}

save :: ToJSON a => BS.ByteString -> (a -> Text) -> a -> L a
save kind key obj = do
  let _id = encodeUtf8 $ key obj
  _ <- redis $ do
    R.set (kind <> ":" <> _id) (LBS.toStrict $ encode obj)
    R.sadd kind [_id]
  return obj

getBot :: BotId -> L Bot
getBot _id = do
  mjson <- redis $ R.get (encodeUtf8 $ "bots:" <> _id)
  case mjson of
    Just j -> case decode $ LBS.fromStrict j of
      Just bot -> return bot
      -- TODO: MonadError
      Nothing  -> error "getBot: failed to decode saved bot"
    Nothing -> error "getBot: failed to find"

-- FIXME: this needs to do something if this bot spec is already running (reboot it?), not boot up two copies
runBot :: MonadIO m => BotSpec m -> m ()
runBot spec@BotSpec{..} = do
  Log.bootBot spec
  bootBot botAdapter spec

botDirectives :: MonadIO m => BotSpec m -> Message -> m ()
botDirectives b msg = do
  let applicable = L.filter (\p -> handlerApplies p b msg) (botHandlers b)

  -- TODO:
  -- * ensure that plugins run in isolation and crash safely
  -- * enforce only one match?
  unless (null applicable) $ do
    let names = L.map handlerName applicable
    Log.handlerMatch (botRecord b) msg names
    forM_ applicable $ \p -> runHandler p b msg

buildBot :: Adapter m -> [Plugin m] -> Bot -> BotSpec m
buildBot botAdapter botPlugins botRecord = BotSpec{..}
