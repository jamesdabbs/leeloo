{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Logging
  ( Logger
  , apiCall
  , bootBot
  , newLogger
  , pluginMatch
  ) where

import Base
import Plugin (BotSpec(..))

import Control.Monad.Logger (MonadLogger(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ANSI
import System.Log.FastLogger

type Logger = FastLogger

-- TODO: what's the right way to handle this?
instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

newLogger :: IO Logger
newLogger = do
  (l, _) <- newFastLogger $ LogStderr defaultBufSize
  return l

pluginMatch :: MonadIO m => Bot -> Message -> [Text] -> m ()
pluginMatch Bot{..} msg = mapM_ log
  where
    log name = blog botName
      [ colorize Magenta (userName . messageUser $ msg)
      , ": "
      , colorize Blue (messageText msg)
      , " => "
      , colorize Green name
      ]

apiCall :: MonadIO m => BotName -> Text -> a -> m ()
apiCall bot endpoint _ = blog bot [ colorize Red endpoint ]

bootBot :: MonadIO m => BotSpec m -> m ()
bootBot BotSpec{..} = blog (botName botRecord) [ "Booting" ]

blog :: MonadIO m => BotName -> [Text] -> m ()
blog bot msg = liftIO . T.putStrLn . T.concat $
  [ "["
  , colorize Green bot
  , "] "
  , T.concat msg
  ]

colorize :: Color -> Text -> Text
colorize color str = T.concat
  [ T.pack $ setSGRCode [SetColor Foreground Vivid color]
  , str
  , T.pack $ setSGRCode [Reset]
  ]
