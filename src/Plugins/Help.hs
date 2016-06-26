{-# LANGUAGE OverloadedStrings #-}
module Plugins.Help
  ( help
  ) where

import Plugins.Base
import Data.Attoparsec.Text

import Plugin (BotSpec(..), handlerCommandOnly, handlerExamples)

import qualified Data.Text as T

help :: BotM m => Plugin m
help = Plugin "help" [helpH]

helpH :: BotM m => Handler m
helpH = mkHandler "help" True (string "help")
  [ Example "help" "Show this help message"
  ] $ \_ -> do
    bot <- getBot
    let examples = concatMap (full handlerExamples bot) $ botHandlers bot :: [(Text, Text)]
        colWidth = maximum $ map (T.length . fst) examples
        msg = T.concat $ concatMap (\(cmd, desc)-> [T.justifyLeft colWidth ' ' cmd, " => ", desc, "\n"]) examples
    reply $ "```\n" <> msg <> "```"

full :: (Handler m -> [Example]) -> BotSpec m -> Handler m -> [(Text, Text)]
full f BotSpec{..} h = map expand $ f h
  where
    expand Example{..} = if handlerCommandOnly h
      then ("@" <> botName botRecord <> ": " <> exampleText, exampleDescription)
      else (exampleText, exampleDescription)
