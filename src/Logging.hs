{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Logging
  ( Logger
  , newLogger
  ) where

import Base

import Control.Monad.Logger (MonadLogger(..))
import System.Log.FastLogger

-- TODO: what's the right way to handle this?
instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

newLogger :: IO Logger
newLogger = do
  (l, _) <- newFastLogger $ LogStderr defaultBufSize
  return l
