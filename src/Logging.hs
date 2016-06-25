{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Logging
  ( Logger
  , apiCall
  , bootBot
  , blog
  , bracket
  , colorize
  , newLogger
  , handlerMatch
  , pprint
  , worker
  ) where

import Base
import Plugin (BotSpec(..))

import           Control.Lens               ((^.))
import           Control.Monad.Logger       (MonadLogger(..))
import           Data.Attoparsec.Lazy       (Result(..), parse)
import           Data.Aeson                 (json')
import           Data.Aeson.Lens            (_Bool, key)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Network.Wreq               (Response, responseBody, responseStatus, statusCode)
import           System.Console.ANSI
import           System.Log.FastLogger

type Logger = FastLogger

-- TODO: what's the right way to handle this?
instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

newLogger :: IO Logger
newLogger = do
  (l, _) <- newFastLogger $ LogStderr defaultBufSize
  return l

handlerMatch :: MonadIO m => Bot -> Message -> [Text] -> m ()
handlerMatch Bot{..} msg = mapM_ log
  where
    log name = blog botName
      [ colorize Magenta (userName . messageUser $ msg)
      , ": "
      , colorize Blue (messageText msg)
      , " => "
      , colorize Green name
      ]

apiCall :: MonadIO m => BotName -> Text -> Response LBS.ByteString -> m ()
apiCall bot endpoint resp = do
  blog bot [ colorize Red endpoint ]
  -- liftIO . LBS.putStrLn . pprint $ resp ^. responseBody

bootBot :: MonadIO m => BotSpec m -> m ()
bootBot BotSpec{..} = blog (botName botRecord) [ "Booting" ]

blog :: MonadIO m => BotName -> [Text] -> m ()
blog bot msg = liftIO . T.putStrLn . T.concat $
  [ bracket Green bot
  , " "
  , T.concat msg
  ]

worker :: MonadIO m => Text -> m ()
worker msg = liftIO . T.putStrLn . T.concat $
  [ bracket Blue "supervisor"
  , " "
  , msg
  ]

colorize :: Color -> Text -> Text
colorize = colorize' Vivid

colorize' :: ColorIntensity -> Color -> Text -> Text
colorize' int color str = T.concat
  [ T.pack $ setSGRCode [SetColor Foreground int color]
  , str
  , T.pack $ setSGRCode [Reset]
  ]

bracket :: Color -> Text -> Text
bracket c text = T.concat
  [ colorize' Dull  c "["
  , colorize' Vivid c text
  , colorize' Dull  c "]"
  ]

pprint :: LBS.ByteString -> LBS.ByteString
pprint s = case parse json' s of
  Done _ v -> encodePretty v
  _        -> s
