{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude (scanl)
import ClassyPrelude

import Network.HTTP.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Data.Aeson
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Base
import Control.Concurrent.Async.Lifted

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist

import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Data.Random.Shuffle.Weighted
import Data.Random.Source.DevRandom
import Data.Random
import Data.Random.Distribution.Uniform.Exclusive

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Track
    artist Text
    song Text
    album Text
    ttl Int
    deriving Show
Station
    stationId Int
    name Text
    Primary stationId
    deriving Show
TrackStations
    track TrackId
    station StationId
    seen Int
    deriving Show
|]

instance Eq Track where
    (Track a1 s1 al1 _) == (Track a2 s2 al2 _) =
        a1  == a2
     && s1  == s2
     && al1 == al2

instance FromJSON Track where
    parseJSON (Object o) = Track <$>
                           o .: "Line1" <*>
                           o .: "Line2" <*>
                           o .: "Line3" <*>
                           o .: "TimeToLive"

pprintTrack :: Track -> Text
pprintTrack (Track artist song album ttl) =
    "Artist: " <> artist <> "\n" <>
    "Track: " <> song <> "\n" <>
    "Album: " <> album <> "\n" <>
    "TimeToLive: " <> tshow ttl <> "\n"

decodeTrack :: MonadResource m => ConduitM ByteString (Either Text Track) m ()
decodeTrack = CC.map (\str -> case (decode $ fromStrict str) of
                                Nothing -> Left ("The following does not appear to be valid JSON track:\n" <> decodeUtf8 str)
                                Just track -> Right track
                     )

sinkTrack' :: MonadResource m => ConduitM (Either Text Track) o m (Either Text Track)
sinkTrack' = do
  mTrack <- await
  case mTrack of
    Nothing -> return $ Left "End of input"
    Just track -> return track

-- dispTrack :: MonadResource m => ConduitM (Either Text Track) o m ()
-- dispTrack = CC.mapM_ consumeTrack

consumeTrack :: (MonadBase IO m, MonadIO m) => TQueue (Track, StationId) -> Either Text Track -> Maybe Track -> StationId -> m (Maybe Track)
consumeTrack _ (Left errMsg) t _ = putStrLn errMsg >> return t
consumeTrack trackQ (Right track) mTrack stationID = do
  newTrack <- case mTrack of
    Nothing -> do
      atomically $ writeTQueue trackQ (track, stationID)
      return track
    Just oldTrack -> case oldTrack == track of
                       True -> return oldTrack
                       False -> do
                            atomically $ writeTQueue trackQ (track, stationID)
                            return track    

  threadDelay $ trackTtl track * 1000
  return $ Just newTrack

baseURL :: Textual t => t
baseURL = "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/"

mainLoop :: (MonadResource m, MonadResourceBase m) => TQueue (Track,StationId) -> Request -> Manager -> StationId -> m ()
mainLoop trackQ request manager stationID = go Nothing
    where
      go mTrack = do
        response <- http request manager
        eTrack <- (responseBody response $$+- decodeTrack =$ sinkTrack')

        newTrack <- consumeTrack trackQ eTrack mTrack stationID

        go newTrack

addToDB :: (MonadResource m, MonadResourceBase m) => TQueue (Track,StationId) -> ReaderT SqlBackend m ()
addToDB trackQ = forever $ do
  (track, stationId) <- atomically $ readTQueue trackQ
  mTrack <- selectFirst [TrackArtist ==. trackArtist track, TrackSong ==. trackSong track] []
  case mTrack of
    Nothing -> do
        tId <- insert track
        insert_ $ TrackStations tId stationId 1
    Just t -> do
      mStations <- selectFirst [TrackStationsTrack ==. entityKey t, TrackStationsStation ==. stationId] []
      mapM_ (\stations -> update (entityKey stations) [TrackStationsSeen +=. 1]) mStations

  transactionSave
  putStrLn $ pprintTrack track

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  q <- newTQueueIO
  _ <- concurrently (
                runSqlite "/tmp/tracks.db" $ do
                  runMigration migrateAll
                  addToDB q
               )
               ( runResourceT $ do
                 requests <- mapM (\stationID -> parseRequest $ baseURL <> show stationID) stations
                 _ <- mapConcurrently (\(stationID, req) -> mainLoop q req manager (StationKey stationID)) $ zip stations requests
                 return ()
               )
  return ()

stations :: [Int]
stations = [44,3,117,2,4,6,14,22,27,32,35,36,38,39,40,47,48,150]

data TrackForStation = TrackForStation Text Text Int
  deriving (Show, Eq, Ord)

createTrackForStation :: (E.Value Text, E.Value Text, E.Value Int) -> TrackForStation
createTrackForStation (E.Value artist, E.Value song, E.Value seen) = TrackForStation artist song seen

addTrack :: TrackForStation -> TrackForStation -> TrackForStation
addTrack (TrackForStation _ _ a) (TrackForStation artist song b) = TrackForStation artist song (a + b)

tracksCDF :: [TrackForStation] -> Map Int TrackForStation
tracksCDF [] = mempty
tracksCDF (track:tracks) = cdfMapFromList $ fmap (\t -> (f t, t)) cdf
    where
      cdf = scanl addTrack track tracks
      f (TrackForStation _ _ n) = n

weightedChoices :: (Num w, Ord w, Distribution Uniform w, Excludable w, Applicative t, Monoid (t a), Eq a, Semigroup (t a))
                => Map w a
                -> Int
                -> RVar (t a)
weightedChoices m n = go m n
    where
      go _ 0 = return mempty
      go m n
         | m == mempty = return mempty
         | otherwise = do
            (newMap, val) <- weightedChoiceExtractCDF m
            vals <- go newMap (n - 1)
            return $ pure val <> vals

getSamples :: (Applicative t, Monoid (t TrackForStation), Semigroup (t TrackForStation), RandomSource m DevRandom) => Map Int TrackForStation -> Int -> m (t TrackForStation)
getSamples cdfM n = runRVar (weightedChoices cdfM n) DevRandom

getTrackForStation
  :: (MonadIO m, MonadBaseControl IO m)
     => E.SqlExpr (E.Value (Key Station))
     -> m [(E.Value Text, E.Value Text, E.Value Int)]
getTrackForStation stationId =
    runSqlite "/tmp/tracks.db"
      $ E.select
      $ E.from $ \(track `E.InnerJoin` station) -> do
          E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
          E.where_ $ station ^. TrackStationsStation E.==. stationId
          return
            ( track ^. TrackArtist
            , track ^. TrackSong
            , station ^. TrackStationsSeen
            )