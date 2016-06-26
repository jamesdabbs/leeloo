{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( allSavedBots
  , botDirectives
  , botToAppUser
  , buildBot
  , getBots
  , redis
  , saveAppUser
  , saveBot
  ) where

import Base
import App
import Plugin
import qualified Logging as Log

import           Control.Exception.Lifted (try)
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

allSavedBots :: L [Bot]
allSavedBots = redis (R.smembers "bots") >>= getBots

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

getBots :: [BS.ByteString] -> L [Bot]
getBots _ids = do
  mjsons <- redis . R.mget $ map encodeKey _ids
  mapM decodeBot mjsons
  where
    encodeKey _id = "bots:" <> _id
    decodeBot mjson = case mjson of
      Nothing -> throwError NotFound
      Just  j -> case decode $ LBS.fromStrict j of
        Just bot -> return bot
        Nothing  -> throwError NotFound


botDirectives :: (MonadBaseControl IO m, MonadIO m) => BotSpec m -> Message -> m ()
botDirectives b msg = do
  let applicable = L.filter (\p -> handlerApplies p b msg) (botHandlers b)

  -- TODO:
  -- * enforce only one match?
  -- * respond to _direct_ messages if nothing matches
  unless (null applicable) $ do
    let names = L.map handlerName applicable
    Log.handlerMatch (botRecord b) msg names
    forM_ applicable $ \p -> tryRun p b msg

tryRun :: (MonadBaseControl IO m, MonadIO m) => Handler m -> BotSpec m -> Message -> m ()
tryRun h b m = do
  result <- try $ runHandler h b m
  case result of
    -- TODO: respond to user if handler crashes
    Left err -> Log.handlerCrash (botRecord b) err
    Right  _ -> return ()

buildBot :: Adapter m -> [Plugin m] -> Bot -> BotSpec m
buildBot botAdapter botPlugins botRecord = BotSpec{..}
