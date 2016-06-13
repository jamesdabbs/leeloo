module Adapters.CLI
  ( adapter
  ) where

import Base
import Bot.Logic (botDirectives)
import Plugins.Base (whitespace)

import           Data.Attoparsec.Text
import           Data.Maybe     (isJust)
import qualified Data.List      as L
import qualified Data.Text.IO   as T
import qualified Data.Text.Lazy as TL
import           System.IO

adapter :: Adapter L
adapter = Adapter
  { bootBot        = _bootBot
  , sendMessage    = _sendMessage
  , parseCommand   = _parseCommand
  , getRoomByName  = _getRoomByName
  , getRoomMembers = _getRoomMembers
  }

_bootBot :: Entity Bot -> L ()
_bootBot (Entity _ bot@Bot{..}) = do
  liftIO . putStrLn $ "Starting " ++ show botName
  let
    loop :: L ()
    loop = do
      input <- liftIO $ do
        T.putStr $ TL.toStrict botName <> " (here) > "
        hFlush stdout
        T.getLine
      unless (input == "q") $ do
        let msg = Message { messageSource = SourceRoom "here", messageText = input }
        botDirectives adapter bot msg
        loop
  loop

_sendMessage :: Bot -> Source -> Text -> L ()
_sendMessage Bot{..} source text =
  liftIO . T.putStrLn $ target <> "> " <> TL.toStrict botName <> ": " <> text
  where
    target = case source of
      SourceRoom room     -> "room:"   <> room
      SourceUser User{..} -> "direct:" <> userName

_parseCommand :: Bot -> Message -> Maybe Text
_parseCommand bot Message{..} = case parseOnly (commandParser bot) messageText of
  Left   _ -> Nothing
  Right mt -> mt

_getRoomByName :: Bot -> Text -> L (Maybe Source)
_getRoomByName _ name = return $ fst <$> L.find f rooms
  where
    f (SourceRoom room, _) = name == room
    f _ = False

_getRoomMembers :: Bot -> Source -> L [User]
_getRoomMembers _ source = do
  let found = L.find (\(s,_) -> s == source) rooms
  return $ case found of
    Just (_, users) -> users
    Nothing         -> []


commandParser :: Bot -> Parser (Maybe Text)
commandParser Bot{..} = do
  direct <- optional $ string "dm:"
  whitespace
  name <- optional $ string ("@" <> TL.toStrict botName)
  whitespace
  rest <- takeText
  return $ if isJust direct || name == (Just $ TL.toStrict botName)
    then Just rest
    else Nothing

me, you :: User
me  = User "1" "me"
you = User "2" "you"

here, there :: Source
here  = SourceRoom "here"
there = SourceRoom "there"

rooms :: [(Source, [User])]
rooms =
  [ (here,  [me, you])
  , (there, [you])
  ]
