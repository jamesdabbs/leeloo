{-# LANGUAGE FlexibleContexts #-}
module Plugins.Panic
  ( panic
  ) where

import Base
import Replicant
import Replicant.Plugins.Base

import           Data.Aeson
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import qualified Database.Redis.Namespace as R

panic :: Replicant e m => Plugin m
panic = Plugin "panic" [check, record, export]

check :: Replicant e m => Handler m
check = mkHandler "Start a panic score poll" True checkP [] $ \mroom -> do
  reply "I don't know, but I'll ask them"
  getRoom mroom >>= \case
    Just roomId -> do
      user <- getSender
      createPoll user roomId >>= deliverPoll
    Nothing -> reply "Sorry ... I couldn't figure out what room you meant"

checkP :: Parser (Maybe Text)
checkP = do
  _ <- string "how"
  _ <- manyTill anyChar (string "every" *> word)
  whitespace
  optional (string "in " *> word)


record :: Replicant e m => Handler m
record = mkHandler "Record reported panic scores" False recordP [] $ \n -> do
  user <- getSender
  activePollFor user >>= \case
    Nothing -> return () -- Should we indicate that there isn't an active poll?
    Just poll -> do
      respondToPoll poll user n
      sendToUser user $ tshow n <> ", got it"

recordP :: Parser Int
recordP = do
  d <- satisfy $ \c -> c >= '1' && c <= '6'
  return $ read [d]


export :: Replicant e m => Handler m
export = mkHandler "Export panic scores" True (string "export panic") [] $ \_ ->
  error "export panic"


getRoom :: Monad m => Maybe Text -> H m (Maybe Room)
getRoom mname = case mname of
  Just name -> getRoomByName name
  _         -> Just . messageRoom <$> getMessage

deliverPoll :: Replicant e m => Poll -> H m ()
deliverPoll Poll{..} = forM_ pollMemberIds $ \u -> sendToUserId u "Hey, how are you doing today (on a scale of 1-6)?"

respondToPoll :: Replicant e m => Poll -> User -> Int -> H m ()
respondToPoll poll user n = do
  when (n > 4) $
    sendToUserId (pollPosterId poll) $ "FYI, @" <> userName user <> " is at a " <> (T.pack $ show n)
  recordPollResponse poll user n

data Poll = Poll
  { pollId        :: UUID.UUID
  , pollPosterId  :: UserId
  , pollMemberIds :: [UserId]
  , pollCreatedAt :: UTCTime
  }

instance ToJSON Poll where
  toJSON Poll{..} = object
    [ "id"         .= UUID.toText pollId
    , "poster_id"  .= pollPosterId
    , "member_ids" .= pollMemberIds
    , "created_at" .= pollCreatedAt
    ]

instance FromJSON Poll where
  parseJSON = withObject "poll" $ \v -> do
    pollId        <- fromJust . UUID.fromText <$> v .: "id" -- should we be concerned here?
    pollPosterId  <- v .: "poster_id"
    pollMemberIds <- v .: "member_ids"
    pollCreatedAt <- v .: "created_at"
    return Poll{..}

data PollResults = PollResults
  { prPoll      :: Poll
  , prResponses :: M.Map UserId Int
  }

createPoll :: Replicant e m => User -> Room -> H m Poll
createPoll user source = do
  pollId  <- liftIO UUID.nextRandom
  members <- map userId <$> getRoomMembers source
  now     <- liftIO getCurrentTime
  let poll = Poll pollId (userId user) members now

  redis $ do
    R.set ("polls:" <> UUID.toASCIIBytes pollId) (LBS.toStrict $ encode poll)
    R.hmset "active-poll" $ map (\m -> (encodeUtf8 m, UUID.toASCIIBytes pollId)) members

  return poll

activePollFor :: Replicant e m => User -> H m (Maybe Poll)
activePollFor User{..} = do
  mpollId <- redis $ R.hget "active-poll" (encodeUtf8 userId)
  case mpollId of
    Nothing -> return Nothing
    Just pollId -> do
      poll <- redis $ R.get ("polls:" <> pollId)
      case decode . LBS.fromStrict <$> poll of
        Nothing -> return Nothing
        Just p  -> return p


recordPollResponse :: Replicant e m => Poll -> User -> Int -> H m ()
recordPollResponse Poll{..} User{..} score = void . redis $ do
  R.hdel "active-poll" [encodeUtf8 userId]
  R.hset ("polls:" <> UUID.toASCIIBytes pollId <> ":responses") (encodeUtf8 userId) (encodeUtf8 $ tshow score)
