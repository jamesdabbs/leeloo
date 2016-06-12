module Adapters.Slack
  ( adapter
  ) where

import Prelude hiding (takeWhile)
import Base

import qualified Adapters.Slack.Api as S
import qualified Adapters.Slack.Types as S

import           Control.Concurrent   (forkIO)
import           Data.Aeson           (eitherDecode)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as L
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy       as LT
import           Network.Socket       (withSocketsDo)
import qualified Network.WebSockets   as WS
import qualified Wuss                 as WS (runSecureClient)

import Bot.Registry (addBot)
import Bot.Logic    (botDirectives)
import Plugins.Base (whitespace)


adapter :: Adapter L
adapter = Adapter
  { bootBot        = bootSlackBot
  , sendMessage    = sendSlackMessage
  , parseCommand   = parseSlackCommand
  , getRoomByName  = getSlackRoomByName
  , getRoomMembers = getSlackRoomMembers
  }

bootSlackBot :: Entity Bot -> L ()
bootSlackBot eb@(Entity _id bot) = do
  conf <- ask
  void . liftIO . withSocketsDo $ do
    url <- S.getWebsocket bot
    let (domain, path) = T.breakOn "/" . T.drop 6 $ url

    WS.runSecureClient (T.unpack domain) 443 (T.unpack path) $ \conn -> do
      WS.forkPingThread conn 15

      -- TODO: don't hardcode this
      S.sendMessage bot "G087UQUDA" "Reporting for duty"

      pid <- forkIO . forever $ WS.receiveData conn >>= dispatchEvents conf bot
      addBot (bots conf) eb pid

runBot :: AppConf -> Bot -> L a -> IO a
runBot conf _ l = runL conf l >>= \case
  Left  _   -> error "bot handler failed" -- TODO
  Right val -> return val

dispatchEvents :: AppConf -> Bot -> LBS.ByteString -> IO ()
dispatchEvents conf bot msg = runBot conf bot $ case eitherDecode msg of
  Left  err   -> liftIO . T.putStrLn $ "Failed to parse event: " <> T.pack err
  Right event -> withMessages (botDirectives adapter bot) event

toMessage :: S.Message -> Message
toMessage sm = Message
  { messageSource = SourceRoom $ S.messageChannel sm
  , messageText   = S.messageBody sm
  }

fromMessage :: Message -> S.Message
fromMessage m = S.Message
  { S.messageChannel = channelIdForSource $ messageSource m
  , S.messageBody    = messageText m
  , S.messageUser    = Nothing -- FIXME
  }

withMessages :: Monad m => (Message -> m ()) -> S.Event -> m ()
withMessages f (S.MessageEvent m) = f $ toMessage m
withMessages _ _ = return ()

commandParser :: Parser (Text, Text)
commandParser = do
  whitespace
  _ <- string "<@"
  userId <- takeWhile $ \c -> c /= '>'
  _ <- char '>'
  whitespace
  msg <- takeWhile $ const True
  return (userId, msg)

parseSlackCommand :: Bot -> Message -> Maybe Text
parseSlackCommand bot msg@Message{..} =
  case parseOnly commandParser messageText of
    Right (_id, command) ->
      if _id == LT.toStrict (botUserId bot)
        then Just command
        else Nothing
    _ ->
      if isDirect $ fromMessage msg
        then Just messageText
        else Nothing

isDirect :: S.Message -> Bool
isDirect S.Message{..} = channelIsDirect && isFromAHuman
  where
    channelIsDirect = T.isPrefixOf "D" messageChannel
    isFromAHuman    = isJust messageUser -- TODO: improve?

sendSlackMessage :: MonadIO m => Bot -> Source -> Text -> m ()
sendSlackMessage bot = S.sendMessage bot . channelIdForSource

channelIdForSource :: Source -> Text
channelIdForSource (SourceRoom t) = t
channelIdForSource (SourceUser User{..}) = userId

getSlackRoomByName :: MonadIO m => Bot -> Text -> m (Maybe Source)
getSlackRoomByName bot name = do
  channels <- S.getChannels bot
  let found = L.find (\c -> S.channelName c == name) channels
  return $ case found of
    Just ch -> Just . SourceRoom $ S.channelId ch
    _       -> Nothing

getSlackRoomMembers :: MonadIO m => Bot -> Source -> m [User]
getSlackRoomMembers bot source = do
  members <- S.getChannelMembers $ channelIdForSource source
  return $ map memberToUser members

memberToUser :: S.User -> User
memberToUser su = User
  { userId   = S.userId su
  , userName = S.userName su
  }
