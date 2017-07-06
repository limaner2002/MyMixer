{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module MCScraper where

import ClassyPrelude hiding (first)

import Core
import Util
-- import MachineUtils hiding (filter)
import Control.Arrow
import Data.Aeson
import Control.Monad.Trans.Resource hiding (release)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (SqlBackend)

import Control.Concurrent.Lock

import Network.HTTP.Client (newManager, Manager, Request (..), parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString.Streaming.HTTP (http)
import ParseCSV
import OAuth
import Concurrent.Streaming
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BSS

newtype MCTrack = MCTrack {unMCTrack :: (Track, Int)}
    deriving Show

instance FromJSON MCTrack where
    parseJSON (Object o) = MCTrack <$> tuple
        where
          track = Track <$> o .: "Line1"
                        <*> o .: "Line2"
                        <*> o .: "Line3"
                        <*> pure Nothing
          ttl = o .: "TimeToLive"
          tuple = (,) <$> track <*> ttl

instance Eq MCTrack where
  MCTrack (t1, _) == MCTrack (t2, _) = t1 == t2

getDelay :: MCTrack -> Int
getDelay (MCTrack (_, delay)) = delay

getLowStations :: MonadIO m => Int -> E.SqlPersistT m [(E.Value (Key Station), E.Value (Maybe Int))]
getLowStations thresh =
  E.select $ do
    E.from $ \(track `E.InnerJoin` station) -> do
      E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
      E.groupBy (station ^. TrackStationsStation)
      let seen = E.sum_ (station ^. TrackStationsSeen)
      E.having (seen E.<=. E.val (Just thresh))
      return ( station ^. TrackStationsStation
             , seen
             )

getStationSeen :: MonadIO m => Key Station -> E.SqlPersistT m [(E.Value (Maybe Int))]
getStationSeen stationKey =
  E.select $ do
    E.from $ \(track `E.InnerJoin` station) -> do
      E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
      E.where_ $ do
        station ^. TrackStationsStation E.==. (E.val stationKey)
      return ( E.sum_ (station ^. TrackStationsSeen))

scrapeStations :: (MonadResource m, Forall (Pure m), MonadBaseControl IO m) => TChan (ScraperStatus (Track, Key Station)) -> [(Int, Int)] -> m ()
scrapeStations chan stations = do
  req <- parseRequest $ "http://nowplayingservices.musicchoice.com/api/NowPlaying/ttla/"
  mgr <- liftBase $ newManager tlsManagerSettings
  let fs = fmap (\(stationId, numScrape) ->
                    getTrack' mgr stationId numScrape
                    >>> S.map (\t -> (fst (unMCTrack t), StationKey stationId))
                ) stations
  fs' <- concurrentFan' (reqSrc mgr req) fs
  concurrentMerge >>> S.chain print >>> S.map Running >>> S.mapM_ (atomically . writeTChan chan) $ fs'
  atomically $ writeTChan chan Done

reqSrc :: MonadResource m =>
     Manager
     -> Request
     -> S.Stream
          (S.Of Request) m ()
reqSrc mgr req = S.each >>> tokenMgr mgr mcTokUrl mcId mcSecret >>> reqMgr $ repeat req

data ScraperStatus a
  = Running a
  | Done

getTrack' :: MonadResource m => Manager -> Int -> Int -> S.Stream (S.Of Request) m r -> S.Stream (S.Of MCTrack) m ()
getTrack' mgr stationId n = S.map (addId stationId)
  >>> getTrack mgr
  >>> delay (\t -> getDelay t * 1000)
  >>> S.scan holdOld (Nothing, False) id
  >>> S.filter (\(_, b) -> b)
  >>> S.map fst
  >>> S.concat
  >>> S.take n
  where
    holdOld (Nothing, _) new = (Just new, True)
    holdOld (Just old, _) new
      | old == new = (Just new, False)
      | otherwise = (Just new, True)


getTrack :: (MonadResource m, FromJSON a) => Manager -> S.Stream (S.Of Request) m r -> S.Stream (S.Of a) m r
getTrack mgr stream = S.mapM getTrack' >>> S.concat $ stream
  where
    getTrack' req = do
      resp <- trace ("Getting track info for: " <> decodeUtf8 (unpack $ path req)) $ http req mgr
      body <- S.fst' <$> (responseBody >>> BSS.toLazy $ resp)
      return $ eitherDecode body

mcScraper :: Text -> Int -> Int -> IO ()
mcScraper dbPath thresh stopAt = do
  chan <- newTChanIO
  vals <- runSqlite dbPath $ getLowStations thresh
  let vals' = fmap ((unStationKey . getVal) *** (stopAt -)) $ fmap (id *** (fromMaybe stopAt . getVal)) $ ClassyPrelude.filter (isJust . getVal . snd) vals
  _ <- fork $ runResourceT $ scrapeStations chan vals'
  runSqlite dbPath $ addToDB' chan
  where
    getVal (E.Value val) = val

addToDB' :: MonadIO m => TChan (ScraperStatus (Track, Key Station)) -> ReaderT SqlBackend m ()
addToDB' chan = loop
  where
    loop = do
      statusPair <- liftIO . atomically . readTChan $ chan
      case statusPair of
        Running pair -> do
          uncurry addToDB pair
          loop
        Done -> return ()
