{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin
  ( Adapter(..)
  , BotM(..)
  , BotSpec(..)
  , Example(..)
  , Handler
  , HandlerCtx(..)
  , Plugin
  , mkPlugin
  , pluginApplies
  , pluginName
  , runPlugin
  ) where

import Base

import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Data.Attoparsec.Text        (Parser, parseOnly)
import           Data.ByteString             (ByteString)
import           Data.Maybe                  (isNothing)
import qualified Data.Text                   as T
import qualified Database.Redis              as Redis (Connection)


data HandlerCtx m = HandlerCtx
  { handlerBot       :: BotSpec m
  , handlerMessage   :: Message
  , handlerNamespace :: ByteString
  , handlerRedisConn :: Redis.Connection
  }
type Handler m a = ReaderT (HandlerCtx m) m a

data Example = Example
  { exampleTexts       :: [Text]
  , exampleDescription :: Text
  }

data Monad m => Plugin m = Plugin
  { pName     :: Text
  , pExamples :: [Example]
  , pCommand  :: Bool
  -- The odd version of Bool is so we can have a consistent
  -- monadic interface to both these helpers
  -- We always `return ()` if the command check fails
  -- These aren't exposed, so it's not that bad, but ...
  -- TODO: make this more intuitive
  , pTest     :: BotSpec m -> Message -> Maybe ()
  , pRun      :: BotSpec m -> Message -> m ()
  }

-- Idea:
-- * enforce that plugins parse their examples
-- * (optional) enforce that other plugins _don't_ parse
data BotSpec m = BotSpec
  { botRecord  :: !Bot
  , botAdapter :: !(Adapter m)
  , botPlugins :: ![Plugin m]
  }

data Monad m => Adapter m = Adapter
  { bootBot        :: BotSpec m -> m ()
  , sendToRoom     :: Bot -> Room -> Text -> m ()
  , sendToUser     :: Bot -> User -> Text -> m ()
  , parseCommand   :: Bot -> Message -> Maybe Text
  , getRoomByName  :: Bot -> Text -> m (Maybe Room)
  , getRoomMembers :: Bot -> Room -> m [User]
  }

class MonadIO m => BotM m where
  redisPool :: m Redis.Connection

checkParser :: Parser a -> BotSpec m -> Message -> Maybe ()
checkParser parser _ Message{..} =
  case parseOnly parser messageText of
    Right _ -> Nothing
    Left  _ -> Just ()

runParser :: Monad m => Parser a -> (BotSpec m -> Message -> a -> m ()) -> BotSpec m -> Message -> m ()
runParser parser handler bot msg@Message{..} =
  case parseOnly parser messageText of
    Right r -> handler bot msg r
    Left  _ -> return ()

pluginApplies :: Monad m => Plugin m -> BotSpec m -> Message -> Bool
pluginApplies p b m = isNothing $ pTest p b m

pluginName :: Monad m => Plugin m -> Text
pluginName = pName

runPlugin :: Monad m => Plugin m -> BotSpec m -> Message -> m ()
runPlugin = pRun

mkPlugin :: BotM m
         => Text
         -> Bool
         -> Parser a
         -> [Example]
         -> (a -> Handler m ())
         -> Plugin m
mkPlugin name commandOnly parser examples handler = verifyPlugin Plugin
  { pName     = name
  , pExamples = examples
  , pCommand  = commandOnly
  , pTest     = withCommand $ checkParser parser
  , pRun      = withCommand . runParser parser $ runHandler handler
  }
  where
    runHandler :: BotM m => (a -> Handler m ()) -> BotSpec m -> Message -> a -> m ()
    runHandler h b msg a = do
      conn <- redisPool
      let
        Bot{..} = botRecord b
        prefix  = T.takeWhile (/= '.') name
        ctx = HandlerCtx
          { handlerBot       = b
          , handlerMessage   = msg
          , handlerNamespace = encodeUtf8 $ "leeloo:" <> botId <> ":" <> prefix
          , handlerRedisConn = conn
          }
      runReaderT (h a) ctx

    withCommand :: (Monad m, Monad a) => (BotSpec a -> Message -> m ()) -> BotSpec a -> Message -> m ()
    withCommand f b m = case parseCommand (botAdapter b) (botRecord b) m of
      Just text -> f b $ m { messageText = text }
      _ -> -- This didn't match the command format for the given adapter
        if commandOnly
          then return ()
          else f b m

verifyPlugin :: Plugin m -> Plugin m
verifyPlugin = id -- TODO: check that all examples do successfully parse
