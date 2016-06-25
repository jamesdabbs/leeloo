{-# LANGUAGE OverloadedStrings #-}
module Plugins.Help
  ( help
  ) where

import Plugins.Base
import Data.Attoparsec.Text

import Plugin (handlerExamples)

import qualified Data.Text as T

help :: BotM m => Plugin m
help = Plugin "help" [helpH]

helpH :: BotM m => Handler m
helpH = mkHandler "help" True (string "help")
  [ Example "help" "Show this help message"
  ] $ \_ -> do
    bot <- getBot
    let examples = concatMap handlerExamples $ botHandlers bot
        colWidth = maximum $ map (\Example{..} -> T.length exampleText) examples
        msg = T.concat $ concatMap (\Example{..} -> [T.justifyLeft colWidth ' ' exampleText, " => ", exampleDescription, "\n"]) examples
    reply $ "```\n" <> msg <> "```"
