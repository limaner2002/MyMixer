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
-- mcScraper = fail "Not yet implemented."
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

-- mcScraper :: Text -> Int -> Int -> ByteString -> IO ()
-- mcScraper dbPath thresh stopAt token = do
--   l <- runSqlite dbPath (getLowStations thresh)
--   req <- addRequestHeader "Authorization" ("bearer " <> token) <$> parseRequest "http://nowplayingservices.musicchoice.com/api/NowPlaying/ttla/"
--   lock <- new
--   let stations = filter onlyMC $ fmap fst l
--       onlyMC (E.Value k) = unStationKey k /= 1000 && unStationKey k /= 1001
--       fromVal (E.Value v) = v

--   putStrLn "Scraping the following stations"
--   mapM_ (print . unStationKey . fromVal) stations

--   runSqlite dbPath $ do
--     _ <- mapConcurrently (\x -> runKleisli (run_ $ scrapeStation lock req stopAt >>> filterJust >>> evMap (\(MCTrack (t, _)) -> pprintTrack t) >>> machine (printIt lock)) $ fmap (\(E.Value k) -> k) [x]) stations
--     return ()
--       where
--         printIt lock a = do
--           liftIO $ acquire lock
--           putStrLn a
--           liftIO $ release lock

-- scrapeTrack :: MonadResource m => ProcessA (Kleisli m) (Event Request) (Event (Maybe MCTrack))
-- scrapeTrack = proc req -> do
--   rec
--     waitReq <- id *** delayIt >>> mergeEventsR >>> evMap fst -< (req, mPrevTrack <$ req)
--     track <- getTrack -< waitReq
--     mPrevTrack <- dHold Nothing -< track
--   case mPrevTrack of
--     Nothing -> returnA -< track
--     Just prevTrack -> do
--       cmp <- mergeEventsR >>> evMap (uncurry compare) >>> hold True -< (track, mPrevTrack <$ track)
--       case cmp of
--         True -> returnA -< noEvent
--         False -> returnA -< track
--  where
--    getTrack = mkManager >>> makeRequest >>> sourceHttp_ >>> evMap snd >>> machineParser json >>> evMap (join . fmap fromJSON')
--    delayIt = machine (\t -> threadDelay $ getDelay t * 1000)
--    compare Nothing Nothing = True
--    compare (Just (MCTrack (a, _))) (Just (MCTrack (b, _))) = a == b
--    compare _ _ = False

-- scrapeStation :: MonadResource m =>
--   Lock -> 
--   Request -> 
--   Int -> 
--      ProcessA
--        (Kleisli (ReaderT E.SqlBackend m))
--        (Event (Key Station))
--        (Event (Maybe MCTrack))
-- scrapeStation lock req thresh = dSwitch emit restart
--  where
--    emit = proc stationKey -> do
--      -- Stops firing when enough tracks are scraped
--      stationKey' <- yieldForever -< stationKey
--      mSeen <- machine getStationSeen' >>> blockingSource' >>> evMap fromVal >>> hold Nothing -< stationKey'

--      case mSeen of
--        Nothing -> returnA -< (noEvent, noEvent)
--        Just seen ->
--          case seen <= thresh of
--            False -> returnA -< (noEvent, () <$ stationKey')
--            True -> do
--              track <- mkReq req >>> scrapeTrack -< stationKey'
--              updated <- filterJust *** id >>> mergeEventsL >>> machine addIt' -< (track, stationKey')
--              res <- mergeEventsL >>> evMap snd -< (updated, track)

--              returnA -< (res, noEvent)
--    addIt' (MCTrack (track, _), stationKey) = do
--      liftIO $ acquire lock
--      addToDB track stationKey
--      liftIO $ release lock
--    getStationSeen' stationKey = do
--      liftIO $ acquire lock
--      r <- getStationSeen stationKey
--      liftIO $ release lock
--      return r
--    fromVal (E.Value v) = v
--    restart _ = scrapeStation lock req thresh

-- addIt :: MonadIO m => TChan (Track, Key Station) -> TChan (Key Station) -> (MCTrack, Key Station) -> m ()
-- addIt wChan ackChan (MCTrack (track, _), stationKey) = do
--   atomically $ writeTChan wChan (track, stationKey)
--   loop
--     where
--       loop = do
--         stationAck <- atomically $ readTChan ackChan
--         case stationKey == stationAck of
--           True -> return ()
--           False -> do
--             atomically $ writeTChan ackChan stationAck
--             loop
        
-- mkReq :: ArrowApply a => Request -> ProcessA a (Event (Key Station)) (Event Request)
-- mkReq req = evMap mkReq'
--   where
--     mkReq' stationKey = req {path = basePath <> "/" <> idBS stationKey}
--     basePath = path req
--     idBS stationKey = encodeUtf8 (tshow (unStationKey stationKey))

-- mkManager :: MonadIO m => ProcessA (Kleisli m) (Event a) (Event (a, Manager))
-- mkManager = switch mkManager' go
--   where
--     mkManager' = proc input -> do
--       mgr <- machine (const $ liftIO $ newManager tlsManagerSettings) -< () <$ input
--       returnA -< (noEvent, mgr)
--     go mgr = evMap (\a -> (a, mgr))

-- yieldForever :: ArrowApply a => ProcessA a (Event b) (Event b)
-- yieldForever = construct go
--   where
--     go = do
--       b <- await
--       forever (yield b)

-- addRequestHeader headerName val req = req { requestHeaders = (headerName, val) : requestHeaders req }
