module Plugins.Die
  ( die
  ) where

import Plugins.Base
import Data.Attoparsec.Text

die :: BotM m => Plugin m
die = Plugin "die" [dieH]

dieH :: BotM m => Handler m
dieH = mkHandler "die" True "die"
  [ Example "die" "Stop (and hopefully restart) the bot"
  ]
  $ error "Forcing error"
