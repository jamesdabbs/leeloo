module Bot.Slack
  ( sendMessage
  , getWebsocket
  , replyTo
  ) where

import Base
import Model (Bot(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Control.Lens         ((.~), (&), (^.))
import           Data.Aeson.Lens      (_String, key)
import           Network.Wreq         (FormParam, postWith, defaults, param, responseBody, Options, Response)

replyTo :: Bot -> Message -> Text -> IO ()
replyTo bot Message{..} = sendMessage bot messageChannel

sendMessage :: MonadIO m => Bot -> Channel -> Text -> m ()
sendMessage Bot{..} channel body = do
  resp <- slackRequest (LT.toStrict botToken) "chat.postMessage" $
    \p -> p & param "channel"     .~ [channel]
            & param "text"        .~ [body]
            & param "username"    .~ [LT.toStrict botName]
            & param "as_user"     .~ ["false" :: Text]
            & param "icon_emoji"  .~ [LT.toStrict botIcon]
  return ()

getWebsocket :: Bot -> IO Text
getWebsocket Bot{..} = do
  r <- slackRequest (LT.toStrict botToken) "rtm.start" id
  return $ r ^. responseBody . key "url" . _String

slackRequest :: MonadIO m => Text -> Text -> (Options -> Options) -> m (Response LBS.ByteString)
slackRequest token endpoint updater = do
  let opts = defaults
           & param "token" .~ [token]
  let url  = "https://slack.com/api/" <> endpoint
  let form = [] :: [FormParam]
  liftIO $ postWith (updater opts) (T.unpack url) form
