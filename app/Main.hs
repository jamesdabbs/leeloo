-- TODO
-- Flesh out panic checking logic
-- Work on error handling
   -- an error in a handler should send a message to Slack
   -- should _not_ impact any other handlers
   -- _should_ force a restart of the offending bot

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Base
import Api     (server)
import Bot     (newBotRegistry, bootSaved)
import Logging (newLogger)

import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)


runApp :: (AppConf -> IO a) -> IO a
runApp action = do
  registry <- newBotRegistry
  logger   <- newLogger
  withDB $ \db -> liftIO . action $ AppConf db registry logger

main :: IO ()
main = runApp $ \conf -> do
  T.putStrLn "Booting stored bots"
  runL conf bootSaved >>= either (error . show) return

  -- let port = 3000
  -- T.putStrLn $ "Starting server on port " <> (T.pack $ show port)
  -- W.run port . logStdoutDev $ server conf
