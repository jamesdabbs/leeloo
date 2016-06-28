-- TODO
-- Flesh out panic checking logic
-- An error in a handler should send a message to Slack?

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api                (startApi)
import Bots               (mkConf, startCli)
import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  conf <- mkConf
  args <- getArgs

  port <- maybe 3000 read <$> lookupEnv "PORT"

  if null args || head args == "api"
    then startApi port conf
    else startCli conf
