{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Base
import Model
import Bot (boot, newBotRegistry, saveBot, savedBots)

import qualified Data.Text    as T
import qualified Data.Text.IO as T


runApp :: (AppConf -> IO a) -> IO a
runApp action = do
  registry <- newBotRegistry
  withDB $ \db -> liftIO . action $ AppConf db registry

main :: IO ()
main = runApp $ \conf -> do
  let registry = bots conf
  bs <- runL' conf savedBots
  case bs of
    Left err -> return ()
    Right botlist -> forM_ botlist $ boot registry

  T.putStrLn "Press enter key to exit"
  T.getLine
  return ()
