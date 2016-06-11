{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Logging
  ( Logger
  , newLogger
  ) where

import Base

import Control.Monad.Logger (LogLevel(..), MonadLogger(..))
import System.Log.FastLogger

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

newLogger :: IO Logger
newLogger = do
  (l, _) <- newFastLogger $ LogStderr defaultBufSize
  return l
