{-# LANGUAGE Rank2Types #-}
module Plugins.Base
  ( onComment
  , onCommand
  , reply
  , echo
  , help
  , whitespace
  , word
  ) where

import Base

import           Data.Attoparsec.Text
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T

type B a m = MonadIO m => Adapter m -> Bot -> Message -> a -> m ()


reply :: MonadIO m => Adapter m -> Bot -> Message -> Text -> m ()
reply a bot msg@Message{..} text = if messageDirect
  then sendToUser a bot messageUser text
  else sendToRoom a bot messageRoom text

echo :: MonadIO m => Adapter m -> Plugin m
echo a = mkPlugin a "Echo back a string" [] True ("echo " *> takeText) $ reply a

help :: MonadIO m => Adapter m -> Plugin m
help a = mkPlugin a "Display help" [] False (string "help") $ \bot msg _ ->
  reply a bot msg "Should say something helpful here"


onCommand :: MonadIO m => Parser a -> B a m
          -> Adapter m -> Bot -> Message -> m ()
onCommand matcher handler adapter bot msg@Message{..} =
  case parseCommand adapter bot msg of
    Just command ->
      let msg' = msg { messageText = command }
      in onComment matcher handler adapter bot msg'
    _ -> return ()

onComment :: MonadIO m => Parser a -> B a m
          -> Adapter m -> Bot -> Message -> m ()
onComment matcher handler adapter bot msg@Message{..} =
  case parseOnly matcher messageText of
    Right a -> do
      liftIO . putStrLn $ "Running matched handler: " ++ show msg
      handler adapter bot msg a
    Left  _ -> return ()


whitespace :: Parser ()
whitespace = void . many . satisfy $ inClass [' ', '\t', '\n']

word :: Parser Text
word = T.pack <$> many' letter
