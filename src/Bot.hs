{-# LANGUAGE FlexibleContexts #-}
module Bot
  ( allSavedBots
  , botToAppUser
  , getBots
  , saveAppUser
  , saveBot
  ) where

import Base
import App
import Replicant

import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Database.Redis.Namespace as R

instance ToJSON AppUser where
  toJSON AppUser{..} = object
    [ "id"      .= appUserId
    , "name"    .= appUserName
    , "token"   .= appUserToken
    ]

instance FromJSON AppUser where
  parseJSON = withObject "app user" $ \v -> do
    appUserId    <- v .: "id"
    appUserName  <- v .: "name"
    appUserToken <- v .: "token"
    return AppUser{..}

allSavedBots :: L [Bot]
allSavedBots = redis (R.smembers "bots") >>= getBots

saveBot :: Bot -> L ()
saveBot = void . save "bots" botId

saveAppUser :: AppUser -> L AppUser
saveAppUser = save "users" appUserId

botToAppUser :: Bot -> AppUser
botToAppUser Bot{..} =
  let
    appUserName  = botName
    appUserToken = botToken
    appUserId    = botId
  in AppUser{..}

save :: ToJSON a => BS.ByteString -> (a -> Text) -> a -> L a
save kind key obj = do
  let _id = encodeUtf8 $ key obj
  _ <- redis $ do
    R.set (kind <> ":" <> _id) (LBS.toStrict $ encode obj)
    R.sadd kind [_id]
  return obj

getBots :: [BS.ByteString] -> L [Bot]
getBots _ids = do
  mjsons <- redis . R.mget $ map encodeKey _ids
  mapM decodeBot mjsons
  where
    encodeKey _id = "bots:" <> _id
    decodeBot mjson = case mjson of
      Nothing -> throwError NotFound
      Just  j -> case decode $ LBS.fromStrict j of
        Just bot -> return bot
        Nothing  -> throwError NotFound
