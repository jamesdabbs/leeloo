-- TODO
-- Flesh out panic checking logic
-- Work on error handling
   -- an error in a handler should send a message to Slack
   -- should _not_ impact any other handlers
   -- _should_ force a restart of the offending bot

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
