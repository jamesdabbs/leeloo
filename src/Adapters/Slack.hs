-- TODO:
-- * get these updated with the new message, adapter signatures
-- * connect each adapter to Redis, namespaced by bot name
-- * cache userId => user and channelId => channel lookups
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
  , sendToUser     = _sendToUser
  , sendToRoom     = _sendToRoom
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
toMessage sm =
  let r = S.messageChannel sm
      u = maybe "" id $ S.messageUser sm
  in Message
       { messageRoom   = Room { roomId = r, roomName = r }
       , messageUser   = User { userId = u, userName = u }
       , messageText   = S.messageBody sm
       , messageDirect = isDirect sm
       }

--fromMessage :: Message -> S.Message
--fromMessage Message{..} = S.Message
--  { S.messageChannel = channelIdForSource $ messageSource m
--  , S.messageBody    = messageText m
--  , S.messageUser    = Nothing -- FIXME
--  }

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
parseSlackCommand bot Message{..} =
  case parseOnly commandParser messageText of
    Right (_id, command) ->
      if _id == LT.toStrict (botUserId bot)
        then Just command
        else Nothing
    _ ->
      if messageDirect
        then Just messageText
        else Nothing

isDirect :: S.Message -> Bool
isDirect S.Message{..} = channelIsDirect && isFromAHuman
  where
    channelIsDirect = T.isPrefixOf "D" messageChannel
    isFromAHuman    = isJust messageUser -- TODO: improve?

_sendToUser :: MonadIO m => Bot -> User -> Text -> m ()
_sendToUser bot User{..} text = do
  roomId <- getImRoomId userId
  S.sendMessage bot roomId text
  where
    getImRoomId = error "getImRoomId"

_sendToRoom :: MonadIO m => Bot -> Room -> Text -> m ()
_sendToRoom bot Room{..} = S.sendMessage bot roomId

channelToRoom :: S.Channel -> Room
channelToRoom ch = Room
  { roomId   = S.channelId ch
  , roomName = S.channelName ch
  }

getSlackRoomByName :: MonadIO m => Bot -> Text -> m (Maybe Room)
getSlackRoomByName bot text = do
  channels <- S.getChannels bot
  return $ channelToRoom <$> L.find (\c -> text == S.channelName c) channels

getSlackRoomMembers :: MonadIO m => Bot -> Room -> m [User]
getSlackRoomMembers bot Room{..} = do
  members <- S.getChannelMembers bot roomId
  return $ map memberToUser members

memberToUser :: S.User -> User
memberToUser su = User
  { userId   = S.userId su
  , userName = S.userName su
  }
