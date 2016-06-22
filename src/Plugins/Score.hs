{-# LANGUAGE OverloadedStrings #-}
module Plugins.Score
  ( score
  , scoreUp
  , scoreDown
  ) where

import Base
import Plugin
import Plugins.Base

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Database.Redis.Namespace


score, scoreUp, scoreDown :: BotM m => Plugin m

score = mkPlugin "points.show" False ("score " *> word) [] $ \word -> do
  n <- redis $ incrby (encodeUtf8 word) 0
  explain word n

scoreUp = mkPlugin "points.up" False (word <* "++") [] $ \word -> do
  n <- redis $ incrby (encodeUtf8 word) 1
  explain word n

scoreDown = mkPlugin "points.down" False (word <* "--") [] $ \word -> do
  n <- redis $ incrby (encodeUtf8 word) (-1)
  explain word n

explain :: Monad m => Text -> Integer -> Handler m ()
explain word n = reply $ word <> " has " <> T.pack (show n) <> " points"
