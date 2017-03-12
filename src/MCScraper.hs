{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}

module MCScraper where

import ClassyPrelude hiding (first)

import Core
import Util
import MachineUtils hiding (filter)
import Data.Aeson
import Control.Monad.Trans.Resource
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Persist.Sqlite (runSqlite)

import Network.HTTP.Client (newManager, Manager, Request (..), parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import ParseCSV

newtype MCTrack = MCTrack (Track, Int)
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

getDelay :: Maybe MCTrack -> Int
getDelay Nothing = 0
getDelay (Just (MCTrack (_, delay))) = delay

scrapeTrack :: MonadResource m => ProcessA (Kleisli m) (Event Request) (Event (Maybe MCTrack))
scrapeTrack = proc req -> do
  rec
    waitReq <- id *** delayIt >>> mergeEventsR >>> evMap fst -< (req, mPrevTrack <$ req)
    track <- getTrack -< waitReq
    mPrevTrack <- dHold Nothing -< track
  case mPrevTrack of
    Nothing -> returnA -< track
    Just prevTrack -> do
      cmp <- mergeEventsR >>> evMap (uncurry compare) >>> hold True -< (track, mPrevTrack <$ track)
      case cmp of
        True -> returnA -< noEvent
        False -> returnA -< track
 where
   getTrack = mkManager >>> makeRequest >>> sourceHttp_ >>> evMap snd >>> machineParser json >>> evMap (join . fmap fromJSON')
   delayIt = machine (\t -> threadDelay $ getDelay t * 1000)
   compare Nothing Nothing = True
   compare (Just (MCTrack (a, _))) (Just (MCTrack (b, _))) = a == b
   compare _ _ = False

scrapeStation :: MonadResource m =>
  Request -> 
  Int -> 
     ProcessA
       (Kleisli (ReaderT E.SqlBackend m))
       (Event (Key Station))
       (Event (Maybe MCTrack))
scrapeStation req thresh = dSwitch emit restart
 where
   emit = proc stationKey -> do
     -- Stops firing when enough tracks are scraped
     stationKey' <- yieldForever -< stationKey
     mSeen <- machine getStationSeen >>> blockingSource' >>> evMap fromVal >>> hold Nothing -< stationKey'

     case mSeen of
       Nothing -> returnA -< (noEvent, noEvent)
       Just seen ->
         case seen <= thresh of
           False -> returnA -< (noEvent, () <$ stationKey')
           True -> do
             track <- mkReq req >>> scrapeTrack -< stationKey'
             updated <- filterJust *** id >>> mergeEventsL >>> machine addIt' -< (track, stationKey')
             res <- mergeEventsL >>> evMap snd -< (updated, track)

             returnA -< (res, noEvent)
   addIt' (MCTrack (track, _), stationKey) = addToDB track stationKey
   fromVal (E.Value v) = v
   restart _ = scrapeStation req thresh

addIt :: MonadIO m => TChan (Track, Key Station) -> TChan (Key Station) -> (MCTrack, Key Station) -> m ()
addIt wChan ackChan (MCTrack (track, _), stationKey) = do
  atomically $ writeTChan wChan (track, stationKey)
  loop
    where
      loop = do
        stationAck <- atomically $ readTChan ackChan
        case stationKey == stationAck of
          True -> return ()
          False -> do
            atomically $ writeTChan ackChan stationAck
            loop
        
mkReq :: ArrowApply a => Request -> ProcessA a (Event (Key Station)) (Event Request)
mkReq req = evMap mkReq'
  where
    mkReq' stationKey = req {path = basePath <> "/" <> idBS stationKey}
    basePath = path req
    idBS stationKey = encodeUtf8 (tshow (unStationKey stationKey))

mkManager :: MonadIO m => ProcessA (Kleisli m) (Event a) (Event (a, Manager))
mkManager = switch mkManager' go
  where
    mkManager' = proc input -> do
      mgr <- machine (const $ liftIO $ newManager tlsManagerSettings) -< () <$ input
      returnA -< (noEvent, mgr)
    go mgr = evMap (\a -> (a, mgr))

yieldForever :: ArrowApply a => ProcessA a (Event b) (Event b)
yieldForever = construct go
  where
    go = do
      b <- await
      forever (yield b)

getLowStations :: MonadIO m => Int -> E.SqlPersistT m [(E.Value (Key Station), E.Value (Maybe Int))]
getLowStations thresh =
  E.select $ do
    E.from $ \(track `E.InnerJoin` station) -> do
      E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
      E.where_ $ do
        station ^. TrackStationsSeen E.>. (E.val 0)
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

mcScraper :: Text -> Int -> IO ()
mcScraper dbPath thresh = do
  l <- runSqlite dbPath (getLowStations thresh)
  req <- parseRequest "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/"
  let stations = filter onlyMC $ fmap fst l
      onlyMC (E.Value k) = unStationKey k /= 1000 && unStationKey k /= 1001
      fromVal (E.Value v) = v

  putStrLn "Scraping the following stations"
  mapM_ (print . unStationKey . fromVal) stations

  runSqlite dbPath $ runKleisli (run_ $ scrapeStation req thresh >>> filterJust >>> machine print) $ fmap (\(E.Value k) -> k) stations

