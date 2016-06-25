module Bot.Supervisor
  ( Supervisor
  , newSupervisor
  , monitor
  , halt
  , status
  ) where

import           Base
import           Control.Concurrent          (forkFinally, killThread)
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException, fromException)
import qualified Data.Map                    as M
import qualified Data.Text                   as T

import qualified Logging as Log

data Worker = Worker
  { workerJob        :: IO ()
  , workerExited     :: TVar (Either SomeException Bool)
  , workerThread     :: TVar (Maybe ThreadId)
  , workerTerminator :: TVar Bool
  }

data Supervisor a = Supervisor
  { supervisorWorkers :: TVar (M.Map a Worker)
  }

newSupervisor :: IO (Supervisor a)
newSupervisor = Supervisor <$> newTVarIO M.empty

monitor :: Ord a => Supervisor a -> a -> IO b -> IO ()
monitor s@Supervisor{..} key job = do
  halt s key
  w <- mkWorker job
  atomically . modifyTVar supervisorWorkers $ M.insert key w
  runWorker w

halt :: Ord a => Supervisor a -> a -> IO ()
halt Supervisor{..} key =
  (atomically $ pop key supervisorWorkers) >>= mapM_ shutdownWorker

status :: Supervisor a -> IO (M.Map a (Maybe ThreadId))
status Supervisor{..} = atomically $
  readTVar supervisorWorkers >>= mapM (readTVar . workerThread)

mkWorker :: IO b -> IO Worker
mkWorker job = Worker
  <$> pure (void job)
  <*> newTVarIO (Right False)
  <*> newTVarIO Nothing
  <*> newTVarIO False

runWorker :: Worker -> IO ()
runWorker w@Worker{..} = do
  Log.worker "Running"
  thread <- forkFinally workerJob $ handleExit w
  atomically . writeTVar workerThread $ Just thread

-- TODO: check workerTerminator and exception to decide if we should try to reboot
handleExit :: Worker -> Either SomeException () -> IO ()
handleExit Worker{..} exit = do
  Log.worker "Exiting"
  atomically . writeTVar workerExited $ const True <$> exit

shutdownWorker :: Worker -> IO ()
shutdownWorker Worker{..} = do
  Log.worker "Shutting down"
  atomically $ writeTVar workerTerminator True
  readTVarIO workerThread >>= mapM_ killThread

pop :: Ord a => a -> TVar (M.Map a b) -> STM (Maybe b)
pop k tmap = do
  val <- M.lookup k <$> readTVar tmap
  modifyTVar' tmap $ M.delete k
  return val
