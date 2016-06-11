{-# LANGUAGE FlexibleContexts #-}
module Base
  ( module Base
  ) where

import Control.Concurrent           as Base (ThreadId)
import Control.Monad                as Base (forever, forM_, void, when)
import Control.Monad.Reader         as Base (MonadReader, asks)
import Control.Monad.IO.Class       as Base (MonadIO)
import Control.Monad.Trans          as Base (liftIO)
import Control.Monad.Trans.Either   as Base (EitherT, left, right)
import Control.Monad.Trans.Except   as Base (ExceptT)
import Control.Monad.Trans.Resource as Base (MonadBaseControl)
import Data.Monoid                  as Base ((<>))
import Data.Text                    as Base (Text)
import Database.Persist             as Base (Entity(..), Key(..))

import Types as Base


-- TODO: These helpers probably belong elsewhere ...
import Control.Monad.Logger (MonadLogger(..))
import Database.Persist.Sqlite as Sqlite
import Model (migrateAll)

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

withDB :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
       => (ConnectionPool -> m a) -> m a
withDB action = Sqlite.withSqlitePool "db/dev.sqlite3" 1 $ \pool -> do
  Sqlite.runSqlPool (Sqlite.runMigration Model.migrateAll) pool
  action pool

runDB :: (MonadReader AppConf m, Monad m, MonadIO m) => SqlPersistT IO a -> m a
runDB p = asks db >>= liftIO . runSqlPool p