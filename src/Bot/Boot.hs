module Bot.Boot
  ( boot
  ) where

import Base
import Model (Bot(..))

import           Control.Concurrent   (forkIO)
import           Data.Aeson           (eitherDecode)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Network.Socket       (withSocketsDo)
import qualified Network.WebSockets   as WS
import qualified Wuss                 as WS (runSecureClient)

import Bot.Registry (addBot)
import Bot.Logic    (botDirectives)
import Bot.Slack    (getWebsocket, sendMessage)

boot :: BotRegistry -> Entity Bot -> IO ()
boot registry eb@(Entity _id bot) = withSocketsDo $ do
  url <- getWebsocket bot
  let (domain, path) = T.breakOn "/" . T.drop 6 $ url

  WS.runSecureClient (T.unpack domain) 443 (T.unpack path) $ \conn -> do
    WS.forkPingThread conn 15

    sendMessage bot "G087UQUDA" "Reporting for duty"

    pid <- forkIO . forever $ dispatchEvents bot conn
    addBot registry eb pid

dispatchEvents :: Bot -> WS.Connection -> IO ()
dispatchEvents bot conn = do
  raw <- WS.receiveData conn
  case eitherDecode raw of
    Left e -> liftIO . T.putStrLn $ "Failed to parse event: " <> T.pack e
    Right event -> botDirectives bot event
