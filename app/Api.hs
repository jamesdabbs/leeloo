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
import Bot.Registry (BotStatus)
import Bots (startSavedBots)
import qualified Controller as C

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

serverT :: ServerT API L
serverT = C.botIndex
     :<|> C.botCreate
     :<|> ( \_id -> C.botStart _id
               :<|> C.botStop _id
          )

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
coerceError = error "coercing error"

startApi :: Int -> AppConf -> IO ()
startApi port conf = do
  runL conf startSavedBots
  T.putStrLn $ "Starting server on port " <> T.pack (show port)
  W.run port . logStdoutDev $ server conf
