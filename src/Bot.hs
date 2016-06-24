{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( botDirectives
  , botToAppUser
  , buildBot
  , getBot
  , getStatuses
  , newBotRegistry
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

redis :: RedisNS R.Redis (Either R.Reply) a -> L (Either R.Reply a)
redis c = do
  conn <- asks redisConn
  liftIO $ runRedisNS conn "leeloo" c

savedBots :: L [Bot]
savedBots = do
  eids <- redis $ R.smembers "bots"
  case eids of
    Left _ -> return []
    Right ids -> mapM (getBot . decodeUtf8) ids -- TODO: mget?

saveBot :: Bot -> L ()
saveBot b = void $ save "bots" botId b

saveAppUser :: AppUser -> L AppUser
saveAppUser u = save "users" appUserId u

botToAppUser :: Bot -> AppUser
botToAppUser Bot{..} =
  let
    appUserName  = botName
    appUserToken = botToken
    appUserId    = botId
  in AppUser{..}

save :: ToJSON a => BS.ByteString -> (a -> Text) -> a -> L a
save kind key obj = do
  let id = encodeUtf8 $ key obj
  redis $ do
    R.set (kind <> ":" <> id) (LBS.toStrict $ encode obj)
    R.sadd kind [id]
  return obj

getBot :: BotId -> L Bot
getBot _id = do
  ejson <- redis $ R.get (encodeUtf8 $ "bots:" <> _id)
  case ejson of
    Right (Just json) -> case decode $ LBS.fromStrict json of
      Just bot -> return bot
      -- TODO: MonadError
      Nothing  -> error "getBot: failed to decode saved bot"
    Right _    -> error "getBot: id not found"
    Left _     -> error "getBot: failed to search"

-- FIXME: this needs to do something if this bot spec is already running (reboot it?), not boot up two copies
runBot :: MonadIO m => BotSpec m -> m ()
runBot spec@BotSpec{..} = do
  Log.bootBot spec
  bootBot botAdapter spec

botDirectives :: MonadIO m => BotSpec m -> Message -> m ()
botDirectives b msg = do
  let applicable = L.filter (\p -> pluginApplies p b msg) (botPlugins b)

  -- TODO:
  -- * ensure that plugins run in isolation and crash safely
  -- * enforce only one match?
  unless (null applicable) $ do
    let names = L.map pluginName applicable
    Log.pluginMatch (botRecord b) msg names
    forM_ applicable $ \p -> runPlugin p b msg

buildBot :: Adapter m -> [Plugin m] -> Bot -> BotSpec m
buildBot botAdapter botPlugins botRecord = BotSpec{..}
