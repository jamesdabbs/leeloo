{-# LANGUAGE FlexibleContexts #-}
module Bots
 ( buildSlackBot
 , defaultPlugins
 , getBot
 , getStatuses
 , mkConf
 , registerBot
 , registerUser
 , savedBots
 , startBot
 , stopBot
 , startCli
 , startSavedBots
 , userFromToken
 , welcomeUser
 ) where

import Base
import Replicant hiding (startBot)
import Replicant.Bot.Supervisor (status)
import App
import Bot

import Plugins.Panic
--import qualified Logging as Log

import qualified Replicant                    as R
import qualified Replicant.Adapters.CLI       as CLI
import qualified Replicant.Adapters.Slack     as Slack
import qualified Replicant.Adapters.Slack.Api as Slack

import qualified Data.ByteString          as BS
import qualified Data.List                as L
import qualified Data.Map                 as M
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import qualified Database.Redis.Namespace as R

startCli :: AppConf -> IO ()
startCli conf = do
  result <- runL conf $ do
    let b = Bot { botId     = "B01"
                , botName   = "leeloo"
                , botIcon   = "^_^"
                , botToken  = "_token_"
                , botUserId = "B01"
                }
    startBot $ buildBot CLI.adapter defaultPlugins b
    CLI.wait
  either (error . show) return result

defaultPlugins :: [Plugin L]
defaultPlugins = [help, echo, score, panic, divide]


buildSlackBot :: Bot -> BotSpec L
buildSlackBot = buildBot Slack.adapter defaultPlugins

startSavedBots :: L ()
startSavedBots = allSavedBots >>= mapM_ (startBot . buildSlackBot)

startBot :: BotSpec L -> L ()
startBot spec = do
  conf <- ask
  R.startBot (void . runL conf) spec

getStatuses :: [Bot] -> L [BotStatus]
getStatuses bs = do
  stats <- supervisor >>= liftIO . status
  return $ map (\b@Bot{..} -> BotStatus b $ M.lookup botId stats) bs

userFromToken :: AuthToken -> L (Maybe AppUser)
userFromToken token = do
  liftIO $ putStrLn $ "Token is " ++ show token
  redis (R.hget "token-users" token) >>= \case
    Nothing  -> return Nothing
    Just uid -> getUser uid

getUser :: BS.ByteString -> L (Maybe AppUser)
getUser uid = return . Just $ AppUser (decodeUtf8 uid) "FIXME: username" "FIXME: usertoken"

registerUser :: AppUserToken -> L (AppUser, AuthToken)
registerUser token = do
  info  <- Slack.getBotInfo $ BotInfo token ""
  user  <- saveAppUser $ botToAppUser info
  token <- generateToken user
  return (user, token)

generateToken :: AppUser -> L AuthToken
generateToken AppUser{..} = do
  token <- UUID.toASCIIBytes <$> liftIO UUID.nextRandom
  redis $ do
    let uid = encodeUtf8 appUserId
    R.hset "token-users" token uid
    R.hset "user-tokens" uid token
  return token


registerBot :: AppUser -> BotToken -> L Bot
registerBot AppUser{..} token = do
  bot <- Slack.getBotInfo $ BotInfo token "pig"
  saveBot bot
  redis $ do
    let bid = encodeUtf8 $ botId bot
        uid = encodeUtf8 appUserId
    R.hset "bot-owners" bid uid
    R.sadd ("owned-bots:" <> uid) [bid]
  startBot $ buildSlackBot bot
  return bot

welcomeUser :: Bot -> AppUser -> L ()
welcomeUser bot AppUser{..} = do
  Slack.sendMessage bot channel $ "Hi, " <> appUserName <> ". Thanks for the invite!"
  Slack.sendMessage bot channel $ "To get started and see what I can do, try saying `@" <> botName bot <> ": help` in a channel"
  where
    channel = "@" <> appUserName

savedBots :: AppUser -> L [Bot]
savedBots AppUser{..} = do
  ids <- redis . R.smembers $ "owned-bots:" <> encodeUtf8 appUserId
  getBots ids

getBot :: AppUser -> BotId -> L Bot
getBot user _id = do
  bots <- savedBots user
  case L.find (\b -> _id == botId b) bots of
    Nothing -> throwError NotFound
    Just  b -> return b
