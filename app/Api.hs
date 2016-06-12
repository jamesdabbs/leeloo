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
  ) where

import Servant

import Base
import qualified Controller as C
import Model

import qualified Network.Wai as W

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
