module Bot.Logic
  ( botDirectives
  ) where

import Base
import Model (Bot(..))

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT

import Bot.Slack (replyTo)

echo :: Bot -> Event -> IO ()
echo bot@Bot{..} (MessageEvent m@Message{..}) = case messageUser of
    Just user -> do
      T.putStrLn $ (LT.toStrict botName) <> ": got message " <> messageBody <> " from " <> user
      replyTo bot m "reply"
    Nothing -> return ()
echo _ m = print m

botDirectives :: Bot -> Event -> IO ()
botDirectives = echo
