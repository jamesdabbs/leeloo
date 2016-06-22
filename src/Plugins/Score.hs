{-# LANGUAGE OverloadedStrings #-}
module Plugins.Score
  ( score
  , scoreUp
  , scoreDown
  ) where

import Base
import Plugin
import Plugins.Base

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Database.Redis.Namespace


score, scoreUp, scoreDown :: MonadIO m => Plugin m

score = mkPlugin "Show points" False ("show " *> word) [] $ \word -> do
  n' <- redis $ get (encodeUtf8 word)
  let n = maybe 0 (read . BS.unpack) n'
  explain word n

scoreUp = mkPlugin "Add points" False ("add " *> word) [] $ \word -> do
  n <- redis $ incrby (encodeUtf8 word) 1
  explain word n

scoreDown = mkPlugin "Add points" False ("remove " *> word) [] $ \word -> do
  n <- redis $ incrby (encodeUtf8 word) (-1)
  explain word n

explain word n = reply $ word <> " has " <> T.pack (show n) <> " points"
