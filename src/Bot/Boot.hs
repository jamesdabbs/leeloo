module Bot.Boot
  ( boot
  ) where

import Base
import Model (Bot(..))

import           Control.Concurrent   (forkIO)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Network.Socket       (withSocketsDo)
import qualified Network.WebSockets   as WS
import qualified Wuss                 as WS (runSecureClient)

import Bot.Registry (addBot)
import Bot.Logic    (botDirectives)
import Bot.Slack    (getWebsocket, sendMessage)

boot :: AppConf -> Entity Bot -> L ()
boot conf eb@(Entity _id bot) = void . liftIO . withSocketsDo $ do
  url <- getWebsocket bot
  let (domain, path) = T.breakOn "/" . T.drop 6 $ url

  WS.runSecureClient (T.unpack domain) 443 (T.unpack path) $ \conn -> do
    WS.forkPingThread conn 15

    sendMessage bot "G087UQUDA" "Reporting for duty"

    pid <- forkIO . forever $ WS.receiveData conn >>= dispatchEvents conf bot
    addBot (bots conf) eb pid

runBot :: AppConf -> Bot -> L a -> IO a
runBot conf _ l = runL conf l >>= \case
  Left err  -> error "bot handler failed" -- TODO
  Right val -> return val

dispatchEvents :: AppConf -> Bot -> LBS.ByteString -> IO ()
dispatchEvents conf bot msg = runBot conf bot $
  case eitherDecode msg of
    Left e -> liftIO . T.putStrLn $ "Failed to parse event: " <> T.pack e
    Right event -> botDirectives bot event
