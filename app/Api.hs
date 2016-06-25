{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Api
  ( server
  , startApi
  ) where

import Servant

import Base
import App
import Bots (startSavedBots)
import qualified Controller as C
import Plugin

import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

type GET    a = Get    '[JSON] a
type POST   a = Post   '[JSON] a
type DELETE a = Delete '[JSON] a

type API = "bots" :> GET [BotStatus]
      :<|> "bots" :> ReqBody '[JSON] BotInfo :> POST ()
      :<|> "bots" :> Capture "bot_id" BotId
           :> ( POST   ()
           :<|> DELETE ()
           )
      :<|> "plugins" :> GET [PluginData]
      :<|> "users" :> "callback" :> QueryParam "code" Text :> GET ()

serverT :: ServerT API L
serverT = C.botIndex
     :<|> C.botCreate
     :<|> ( \_id -> C.botStart _id
               :<|> C.botStop _id
          )
     :<|> C.pluginIndex
     :<|> C.oauthCallback

server :: AppConf -> W.Application
server = serve api . extend serverT

api :: Proxy API
api = Proxy

extend :: ServerT API L -> AppConf -> Server API
extend handlers conf = enter (Nat $ run conf) handlers

run :: AppConf -> L a -> ExceptT ServantErr IO a
run conf l = liftIO (runL conf l) >>= \case
  Left  err -> throwError $ coerceError err
  Right val -> return val

coerceError :: AppError -> ServantErr
coerceError (Redirect url) = err303 { errHeaders = [("Location", encodeUtf8 url)] }
coerceError  NotFound      = err404
coerceError  Invalid       = err400

startApi :: Int -> AppConf -> IO ()
startApi port conf = do
  runL conf startSavedBots
  T.putStrLn $ "Starting server on port " <> T.pack (show port)
  W.run port . logStdoutDev $ server conf
