{-# LANGUAGE FlexibleContexts #-}
module Plugin
  ( Adapter(..)
  , BotSpec(..)
  , Example(..)
  , Plugin
  , mkPlugin
  , namespace
  , pluginApplies
  , pluginName
  , runPlugin
  ) where

import Base

import           Control.Monad.Reader        (local)
import           Data.Attoparsec.Text        (Parser, parseOnly)
import           Data.Maybe                  (isNothing)

data Example = Example
  { exampleTexts       :: [Text]
  , exampleDescription :: Text
  }

data Monad m => Plugin m = Plugin
  { pName     :: Text
  , pExamples :: [Example]
  , pCommand  :: Bool
  , pAdapter  :: Adapter m
  -- The odd version of Bool is so we can have a consistent
  -- monadic interface to both these helpers
  -- We always `return ()` if the command check fails
  -- These aren't exposed, so it's not that bad, but ...
  -- TODO: make this more intuitive
  , pTest     :: Bot -> Message -> Maybe ()
  , pRun      :: Bot -> Message -> m ()
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

checkParser :: Parser a -> Bot -> Message -> Maybe ()
checkParser parser bot Message{..} =
  case parseOnly parser messageText of
    Right _ -> Nothing
    Left  _ -> Just ()

runParser :: Monad m => Parser a -> (Bot -> Message -> a -> m ()) -> Bot -> Message -> m ()
runParser parser handler bot msg@Message{..} =
  case parseOnly parser messageText of
    Right r -> handler bot msg r
    Left  _ -> return ()

pluginApplies :: Monad m => Plugin m -> Bot -> Message -> Bool
pluginApplies p b m = isNothing $ pTest p b m

pluginName :: Monad m => Plugin m -> Text
pluginName = pName

runPlugin :: Monad m => Plugin m -> Bot -> Message -> m ()
runPlugin = pRun

namespace :: (MonadReader Namespace m) => Namespace -> m a -> m a
namespace extra = local $ \ns -> ns <> ":" <> extra

mkPlugin :: (MonadReader Namespace m, Monad m)
         => Adapter m
         -> Text
         -> [Example]
         -> Bool
         -> Parser a
         -> (Bot -> Message -> a -> m ())
         -> Plugin m
mkPlugin adapter name examples commandOnly parser handler = verifyPlugin Plugin
  { pName     = name
  , pExamples = examples
  , pCommand  = commandOnly
  , pAdapter  = adapter
  , pTest     = withCommand $ checkParser parser
  , pRun      = withCommand . runParser parser $ namespaced handler
  }
  where
    namespaced handler bot m a = namespace (fullname bot) $ handler bot m a

    fullname bot = botId bot <> ":" <> name

    withCommand :: Monad m => (Bot -> Message -> m ()) -> Bot -> Message -> m ()
    withCommand f b m = case parseCommand adapter b m of
      Just text -> f b $ m { messageText = text }
      _ -> -- This didn't match the command format for the given adapter
        if commandOnly
          then return ()
          else f b m

verifyPlugin :: Plugin m -> Plugin m
verifyPlugin = id -- TODO: check that all examples do successfully parse
