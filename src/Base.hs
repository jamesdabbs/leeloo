{-# LANGUAGE FlexibleContexts #-}
module Base
  ( module Base
  ) where

import Control.Applicative          as Base (optional, many)
import Control.Concurrent           as Base (ThreadId)
import Control.Monad                as Base (MonadPlus, forever, forM_, liftM2, mzero, unless, void, when, (>=>))
import Control.Monad.Logger         as Base (MonadLogger, logDebug, logError, logInfo, logWarn, toLogStr)
import Control.Monad.Reader         as Base (MonadReader, asks, ask)
import Control.Monad.IO.Class       as Base (MonadIO)
import Control.Monad.Trans          as Base (liftIO)
import Control.Monad.Trans.Either   as Base (EitherT, left, right)
import Control.Monad.Trans.Except   as Base (ExceptT)
import Control.Monad.Trans.Resource as Base (MonadBaseControl)
import Data.Monoid                  as Base ((<>))
import Data.Text                    as Base (Text)
import Data.Text.Encoding           as Base (encodeUtf8, decodeUtf8)

import Types as Base

import qualified Debug.Trace as Debug

tr :: Show a => a -> b -> b
tr = Debug.traceShow

tr' :: Show a => a -> a
tr' = Debug.traceShowId

trm :: (Show a, Monad m) => a -> m ()
trm = Debug.traceShowM
