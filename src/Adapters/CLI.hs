module Adapters.CLI
  ( adapter
  , wait
  ) where

import Base
import Bot          (botDirectives)
import Plugins.Base (whitespace)
import qualified Logging as Log

import           Control.Concurrent.MVar
import           Data.Attoparsec.Text
import           Data.Maybe     (isJust)
import qualified Data.List      as L
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Console.ANSI
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import App
import Plugin

adapter :: Adapter L
adapter = Adapter
  { bootBot        = _bootBot
  , sendToUser     = _sendUser
  , sendToRoom     = _sendRoom
  , parseCommand   = _parseCommand
  , getRoomByName  = _getRoomByName
  , getRoomMembers = _getRoomMembers
  }

done :: MVar ()
{-# NOINLINE done #-}
done = unsafePerformIO newEmptyMVar

wait :: MonadIO m => m ()
{-# NOINLINE wait #-}
wait = liftIO $ takeMVar done

_bootBot :: BotSpec L -> L ()
_bootBot spec@BotSpec{..} = do
  let Bot{..} = botRecord
  let
    loop :: L ()
    loop = do
      _log [ Log.bracket botC botName
           , Log.bracket roomC "here"
           , " < "
           ]
      input <- liftIO $ do
        hFlush stdout
        T.getLine
      if input == "q"
        then liftIO $ putMVar done ()
        else do
          case parseOnly messageParser input of
            Left  err -> liftIO . putStrLn $ "Could not parse input: " ++ err
            Right msg -> botDirectives spec msg
          loop
  loop

_log :: MonadIO m => [Text] -> m ()
_log = liftIO . T.putStr . T.concat

outPrompt :: Text
outPrompt = Log.colorize' Dull botC " > "

prefixLines :: Text -> Text -> Text
prefixLines pre corpus = T.unlines . map (\l -> pre <> l) $ T.lines corpus

_send :: Text -> Text -> Text -> L ()
_send bot target text = _log
  [ Log.bracket botC bot
  , target
  , prefixLines outPrompt text
  ]

_sendUser :: Bot -> User -> Text -> L ()
_sendUser Bot{..} User{..} = _send botName $ Log.bracket userC userName

_sendRoom :: Bot -> Room -> Text -> L ()
_sendRoom Bot{..} Room{..} = _send botName $ Log.bracket roomC roomName

_parseCommand :: Bot -> Message -> Maybe Text
_parseCommand bot Message{..} = case parseOnly (commandParser bot) messageText of
  Left   _ -> Nothing
  Right mt -> mt

_getRoomByName :: Bot -> Text -> L (Maybe Room)
_getRoomByName _ = return . roomNamed

_getRoomMembers :: Bot -> Room -> L [User]
_getRoomMembers _ room = do
  let found = L.find (\(r,_) -> r == room) rooms
  return $ case found of
    Just (_, users) -> users
    Nothing         -> []

word :: Parser Text
word = T.pack <$> many' letter

messageParser :: Parser Message
messageParser = do
  mRoomName <- optional $ "room:" *> word
  let mRoom = case mRoomName of
        Just name -> roomNamed name
        Nothing   -> Just here
  room <- maybe mzero return mRoom

  mDirect <- optional "dm:"
  whitespace
  rest <- takeText
  return Message
    { messageRoom   = room
    , messageUser   = me
    , messageText   = rest
    , messageDirect = isJust mDirect
    }


commandParser :: Bot -> Parser (Maybe Text)
commandParser Bot{..} = do
  name <- optional $ string ("@" <> botName)
  whitespace
  rest <- takeText
  return $ if isJust name
    then Just rest
    else Nothing

me, you :: User
me  = User "1" "me"
you = User "2" "you"

here, there :: Room
here  = Room "A" "here"
there = Room "B" "there"

rooms :: [(Room, [User])]
rooms =
  [ (here,  [me, you])
  , (there, [you])
  ]

roomNamed :: Text -> Maybe Room
roomNamed name = fst <$> L.find (\(r,_) -> roomName r == name) rooms

botC, roomC, userC :: Color
botC  = Green
roomC = Yellow
userC = Cyan
