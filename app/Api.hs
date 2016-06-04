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
  ( API
  ) where

import Data.Aeson
import Servant

import Base

type API = "bots" :> Get  '[JSON] [BotSpec]
      :<|> "bots" :> Post '[JSON] ()

api :: Proxy API
api = Proxy

serverT :: ServerT Api L
serverT = error "serverT"

server :: AppConf -> Server Api
server conf = enter lToEither serverT
  where
    lToEither = Nat $ \ar ->
      liftIO ()
