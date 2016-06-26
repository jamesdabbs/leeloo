{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Plugin
  ( Adapter(..)
  , BotM(..)
  , BotSpec(..)
  , Example(..)
  , H
  , Handler
  , HandlerCtx(..)
  , Plugin(..)
  , PluginData(..)
  , botHandlers
  , mkHandler
  , handlerApplies
  , handlerCommandOnly
  , handlerExamples
  , handlerName
  , pluginExamples
  , runHandler
  ) where

import Base

import           Control.Monad.Reader        (ReaderT, runReaderT, lift)
import           Data.Attoparsec.Text        (Parser, parseOnly)
import           Data.ByteString             (ByteString)
import           Data.List                   (find)
import           Data.Maybe                  (isNothing)
import qualified Database.Redis              as Redis (Connection)


data HandlerCtx m = HandlerCtx
  { handlerBot        :: BotSpec m
  , handlerMessage    :: Message
  , handlerPluginName :: ByteString
  , handlerRedisConn  :: Redis.Connection
  }
type H m a = ReaderT (HandlerCtx m) m a

instance BotM m => BotM (ReaderT (HandlerCtx m) m) where
  redisPool = lift redisPool
  redisNamespace = do
    base        <- lift redisNamespace
    plugin      <- asks handlerPluginName
    BotSpec{..} <- asks handlerBot
    return $ base <> ":bot:" <> encodeUtf8 (botId botRecord) <> ":plugin:" <> plugin

data Example = Example
  { exampleText        :: Text
  , exampleDescription :: Text
  } deriving Show

data Plugin m = Plugin
  { pluginName      :: Text
  , pluginHandlers  :: [Handler m]
  }

data PluginData = PluginData
  { pdName     :: Text
  , pdExamples :: [Example]
  }

data Handler m = Handler
  { handlerName     :: Text
  , handlerExamples :: [Example]
  , handlerCommand  :: Bool
  -- The odd version of Bool is so we can have a consistent
  -- monadic interface to both these helpers
  -- We always `return ()` if the command check fails
  -- These aren't exposed, so it's not that bad, but ...
  -- TODO: make this more intuitive
  , handlerTest     :: BotSpec m -> Message -> Maybe ()
  , handlerRun      :: BotSpec m -> Message -> m ()
  }

-- Idea:
-- * enforce that plugins parse their examples
-- * (optional) enforce that other plugins _don't_ parse
data BotSpec m = BotSpec
  { botRecord  :: !Bot
  , botAdapter :: !(Adapter m)
  , botPlugins :: ![Plugin m]
  }

data Adapter m = Adapter
  { bootBot        :: BotSpec m -> m ()
  , sendToRoom     :: Bot -> Room -> Text -> m ()
  , sendToUser     :: Bot -> User -> Text -> m ()
  , parseCommand   :: Bot -> Message -> Maybe Text
  , getRoomByName  :: Bot -> Text -> m (Maybe Room)
  , getRoomMembers :: Bot -> Room -> m [User]
  }

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

handlerApplies :: Monad m => Handler m -> BotSpec m -> Message -> Bool
handlerApplies p b m = isNothing $ handlerTest p b m

handlerCommandOnly :: Handler m -> Bool
handlerCommandOnly = handlerCommand

runHandler :: Monad m => Handler m -> BotSpec m -> Message -> m ()
runHandler = handlerRun

mkHandler :: BotM m
          => Text
          -> Bool
          -> Parser a
          -> [Example]
          -> (a -> H m ())
          -> Handler m
mkHandler name commandOnly parser examples handler = Handler
  { handlerName     = name
  , handlerExamples = map verify examples
  , handlerCommand  = commandOnly
  , handlerTest     = withCommand $ checkParser parser
  , handlerRun      = withCommand . runParser parser $ run handler
  }
  where
    run :: BotM m => (a -> H m ()) -> BotSpec m -> Message -> a -> m ()
    run h b msg a = do
      conn <- redisPool
      let
        Bot{..} = botRecord b
        ctx = HandlerCtx
          { handlerBot        = b
          , handlerMessage    = msg
          , handlerPluginName = encodeUtf8 $ pluginNamespace b name
          , handlerRedisConn  = conn
          }
      runReaderT (h a) ctx

    withCommand :: (Monad m, Monad a) => (BotSpec a -> Message -> m ()) -> BotSpec a -> Message -> m ()
    withCommand f b m = case parseCommand (botAdapter b) (botRecord b) m of
      Just text -> f b $ m { messageText = text }
      _ -> -- This didn't match the command format for the given adapter
        if commandOnly
          then return ()
          else f b m

    -- TODO: check that each example does match the given parser (what about commands, users, room, &c)
    verify = id

botHandlers :: BotSpec m -> [Handler m]
botHandlers bot = concatMap pluginHandlers $ botPlugins bot

-- TODO: plugins should probably just hold on to their namespace. How should
--   we enforce consistency?
pluginNamespace :: BotSpec m -> Text -> Text
pluginNamespace BotSpec{..} handler =
  case find container botPlugins of
    Just plugin -> pluginName plugin
    -- This handler isn't in the bots' installed plugins
    Nothing -> "handler:" <> handler
  where
    container Plugin{..} = any (\h -> handlerName h == handler) pluginHandlers

pluginExamples :: Plugin m -> [Example]
pluginExamples = concatMap handlerExamples . pluginHandlers
