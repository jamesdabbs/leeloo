module Bot.Logic
  ( botDirectives
  ) where

import Base
import Model (Bot(..))

import qualified Data.Text.Lazy as LT

import Bot.Slack (replyTo)

echo :: Bot -> Event -> L ()
echo bot@Bot{..} (MessageEvent m@Message{..}) = case messageUser of
  Just user -> do
    $logInfo $ (LT.toStrict botName) <> ": got message " <> messageBody <> " from " <> user
    liftIO $ replyTo bot m "reply"
  Nothing -> return ()
echo Bot{..} (UnknownEvent t _) = $logDebug $ (LT.toStrict botName) <> ": unknown event " <> t
echo Bot{..} _ = $logDebug $ (LT.toStrict botName) <> ": unhandled event"

botDirectives :: Bot -> Event -> L ()
botDirectives = echo
