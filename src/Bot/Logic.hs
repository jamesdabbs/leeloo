module Bot.Logic
  ( botDirectives
  ) where

import Prelude hiding (takeWhile)

import Base
import Model (Bot(..))
import Types.Slack

import           Data.Attoparsec.Text
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

import Bot.Slack (replyTo)

type B a = Bot -> Message -> a -> L ()

isDirect :: Message -> Bool
isDirect Message{..} = channelIsDirect && isFromAHuman
  where
    channelIsDirect = T.isPrefixOf "D" messageChannel
    isFromAHuman    = isJust messageUser -- TODO: improve?

whitespace :: Parser ()
whitespace = void . many . satisfy $ inClass [' ', '\t', '\n']

parseCommand :: Parser (Text, Text)
parseCommand = do
  whitespace
  _ <- string "<@"
  userId <- takeWhile $ \c -> c /= '>'
  _ <- char '>'
  whitespace
  msg <- takeWhile $ const True
  return (userId, msg)

onCommand :: Parser a -> B a -> Bot -> Message -> L ()
onCommand matcher handler bot msg@Message{..} =
  case parseOnly parseCommand messageBody of
    Right (target, command) ->
      when (LT.fromStrict target == botUserId bot) $ run command
    Left _ -> when (isDirect msg) $ run messageBody
  where
    run cmd = case parseOnly matcher cmd of
      Right a -> handler bot msg a
      Left  _ -> return ()

onComment :: Parser a -> B a -> Bot -> Message -> L ()
onComment matcher handler bot msg@Message{..} =
  case parseOnly matcher messageBody of
    Right a -> handler bot msg a
    Left  _ -> return ()

withMessage :: (Message -> L ()) -> Event -> L ()
withMessage f (MessageEvent m) = f m
withMessage _ _ = return ()

reply :: MonadIO m => Bot -> Message -> Text -> m ()
reply bot message = liftIO . replyTo bot message

echo :: Bot -> Message -> L ()
echo = onComment ("echo " *> takeText) reply

help :: Bot -> Message -> L ()
help = onCommand (string "help") $ \b m _ ->
  reply b m "Should say something helpful here"

botDirectives :: Bot -> Event -> L ()
botDirectives bot = withMessage $ \msg -> do
  echo bot msg
  help bot msg
