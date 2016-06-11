-- TODO
-- Add bot functionality - debug queue
-- Endpoint for creating new bot w/ token

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Base
import Api (server)
import Bot (newBotRegistry, bootSaved)
import Model
import Logging (newLogger)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.RequestLogger (logStdoutDev)


runApp :: (AppConf -> IO a) -> IO a
runApp action = do
  registry <- newBotRegistry
  logger   <- newLogger
  withDB $ \db -> liftIO . action $ AppConf db registry logger

main :: IO ()
main = runApp $ \conf -> do
  T.putStrLn "Booting stored bots"
  runL conf bootSaved >>= \case
    Left err -> error $ show err
    Right _  -> return ()

  T.putStrLn "Starting server"
  W.run 3000 . logStdoutDev $ server conf
