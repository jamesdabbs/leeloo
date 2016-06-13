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
import Database.Persist             as Base (Entity(..), Key(..))

import Types as Base

import qualified Debug.Trace as Debug

-- TODO: These helpers probably belong elsewhere ...
import Database.Persist.Sqlite as Sqlite
import Model (migrateAll)

withDB :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
       => (ConnectionPool -> m a) -> m a
withDB action = Sqlite.withSqlitePool "db/dev.sqlite3" 1 $ \pool -> do
  Sqlite.runSqlPool (Sqlite.runMigration Model.migrateAll) pool
  action pool

runDB :: (MonadReader AppConf m, Monad m, MonadIO m) => SqlPersistT IO a -> m a
runDB p = asks db >>= liftIO . runSqlPool p

tr :: Show a => a -> b -> b
tr = Debug.traceShow

tr' :: Show a => a -> a
tr' = Debug.traceShowId

trm :: (Show a, Monad m) => a -> m ()
trm = Debug.traceShowM
