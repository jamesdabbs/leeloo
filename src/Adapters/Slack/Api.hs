module Adapters.Slack.Api
  ( getBotInfo
  , getWebsocket
  , replyTo
  , sendMessage
  , getChannels
  , getChannelMembers
  ) where

import Base hiding (sendMessage)
import qualified Adapters.Slack.Types as S

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Data.Aeson
import           Data.Aeson.Types            (Parser)
import           Control.Lens         ((.~), (&), (^.))
import           Data.Aeson.Lens      (_String, key)
import           Network.Wreq         (FormParam, postWith, defaults, param, responseBody, Options, Response)

replyTo :: MonadIO m => Bot -> S.Message -> Text -> m ()
replyTo bot S.Message{..} = sendMessage bot messageChannel

sendMessage :: MonadIO m => Bot -> S.ChannelId -> Text -> m ()
sendMessage Bot{..} channel body = do
  resp <- slackRequest botToken "chat.postMessage" $
    \p -> p & param "channel"     .~ [channel]
            & param "text"        .~ [body]
            & param "username"    .~ [botName]
            & param "as_user"     .~ ["false" :: Text]
            & param "icon_emoji"  .~ [botIcon]
  return ()

getWebsocket :: Bot -> IO Text
getWebsocket Bot{..} = do
  r <- slackRequest botToken "rtm.start" id
  return $ r ^. responseBody . key "url" . _String

getBotInfo :: MonadIO m => BotInfo -> m Bot
getBotInfo BotInfo{..} = do
  r <- slackRequest botInfoToken "auth.test" id
  let k str = r ^. responseBody . key str . _String
      botName   = k "user"
      botUserId = k "user_id"
      botToken  = botInfoToken
      botIcon   = ":" <> botInfoIcon <> ":"
      teamId    = k "team_id"
      botId     = "slack:" <> teamId <> ":" <> botUserId
  return Bot{..}

getChannels :: MonadIO m => Bot -> m [S.Channel]
getChannels Bot{..} = do
  error "FIXME: getChannels"
  return []

getChannelMembers :: MonadIO m => Bot -> Text -> m [S.User]
getChannelMembers _ _ = do
  error "FIXME: getChannelMembers"
  return []

slackRequest :: MonadIO m => Text -> Text -> (Options -> Options) -> m (Response LBS.ByteString)
slackRequest token endpoint updater = do
  let opts = defaults
           & param "token" .~ [token]
  let url  = "https://slack.com/api/" <> endpoint
  let form = [] :: [FormParam]
  liftIO $ do
    -- TODO: better API request logging
    print $ "Slack " <> endpoint
    r <- postWith (updater opts) (T.unpack url) form
    print r
    return r

instance FromJSON S.Message where
  parseJSON = withObject "message" $ \v -> do
    messageBody      <- v .: "text"
    messageChannel   <- v .: "channel"
    messageUser      <- v .:? "user"
    return S.Message{..}

instance FromJSON S.Event where
  parseJSON = withObject "event" $ \v -> do
    typ <- v .:? "type"
    case typ of
      Just t  -> parseEvent t $ Object v
      Nothing -> do
        ok <- v .: "ok"
        if ok
          then return S.MessageResponse -- <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else return S.MessageError -- <$> v .: "reply_to" <*> v .: "error"

parseEvent :: Text -> Value -> Parser S.Event
parseEvent t = withObject "event" $ \v ->
  case t of
    "message" -> do
      m <- parseJSON $ Object v
      return $ S.MessageEvent m
    _ -> return $ S.UnknownEvent t (encode v)
