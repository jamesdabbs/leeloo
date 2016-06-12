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
import qualified Data.Text                  as T

type B a m = MonadIO m => Adapter m -> Bot -> Message -> a -> m ()


reply :: MonadIO m => Adapter m -> Bot -> Message -> Text -> m ()
reply Adapter{..} bot Message{..} = sendMessage bot messageSource

echo :: MonadIO m => Adapter m -> Bot -> Message -> m ()
echo = onCommand ("echo " *> takeText) reply

help :: MonadIO m => Adapter m -> Bot -> Message -> m ()
help = onComment (string "help") $ \a b m _ ->
  reply a b m "Should say something helpful here"


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
