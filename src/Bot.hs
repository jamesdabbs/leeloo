{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( botDirectives
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
import qualified Logging as Log

import Bot.Registry (getStatuses, newBotRegistry)

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List  as L
import qualified Database.Redis as R (Redis, Reply)
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
saveBot b = void . redis $ do
  R.set ("bots:" <> id) (LBS.toStrict $ encode b)
  R.sadd "bots" [id]
  where
    id = encodeUtf8 $ botId b

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
