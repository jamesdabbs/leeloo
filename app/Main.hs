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
import System.Environment (getArgs)

main :: IO ()
main = do
  conf <- mkConf
  args <- getArgs

  if null args || head args == "api"
    then startApi 3000 conf
    else startCli conf
