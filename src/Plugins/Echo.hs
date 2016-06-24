{-# LANGUAGE OverloadedStrings #-}
module Plugins.Echo
  ( echo
  ) where

import Plugins.Base
import Data.Attoparsec.Text

echo :: BotM m => Plugin m
echo = Plugin "echo" [echoH]

echoH :: BotM m => Handler m
echoH = mkHandler "echo" False ("echo " *> takeText)
  [ Example "echo hello world" "Repeat `hello world` back"
  ]
  reply
