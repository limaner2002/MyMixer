{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Prelude (scanl)
import ClassyPrelude hiding (replicateM)
import Control.Monad (replicateM)

import Network.HTTP.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Aeson
import Data.Aeson.Types
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

import Util

newtype Artist = Artist Text
    deriving Show

instance FromJSON Artist where
    parseJSON (Object o) = Artist <$> o .: "name"

newtype Album = Album Text
    deriving Show

instance FromJSON Album where
    parseJSON (Object o) = Album <$> o .: "name"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Track
    artist Text
    song Text
    album Text
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
    Primary track station
    deriving Show
|]

newtype TrackSearch t a = TrackSearch (PagingObject t a)
    deriving Show

instance (Traversable t, FromJSON a, FromJSON (t a)) => FromJSON (TrackSearch t a) where
    parseJSON (Object o) = TrackSearch <$> o .: "tracks"

data PagingObject t a where
    PagingObject :: Traversable t => t a -> PagingObject t a

deriving instance (Show (t a), Show a) => Show (PagingObject t a)

instance (Traversable t, FromJSON a, FromJSON (t a)) => FromJSON (PagingObject t a) where
    parseJSON (Object o) = PagingObject <$> o .: "items"

instance Eq Track where
    (Track a1 s1 al1) == (Track a2 s2 al2) =
        a1  == a2
     && s1  == s2
     && al1 == al2

instance FromJSON Track where
    parseJSON (Object o) = Track <$> artists
                                 <*> o .: "name"
                                 <*> album
        where
          artists = artistText <$> headEx <$> (o .: "artists" :: Parser [Artist])
          album = albumText <$> o .: "album"
          artistText (Artist n) = n
          albumText (Album n) = n

newtype MCTrack = MCTrack (Track, Int)
    deriving Show

instance FromJSON MCTrack where
    parseJSON (Object o) = MCTrack <$> tuple
        where
          track = Track <$> o .: "Line1"
                        <*> o .: "Line2"
                        <*> o .: "Line3"
          ttl = o .: "TimeToLive"
          tuple = (,) <$> track <*> ttl

pprintTrack :: Track -> Text
pprintTrack (Track artist song album) =
    "Artist: " <> artist <> "\n" <>
    "Track: " <> song <> "\n" <>
    "Album: " <> album <> "\n"

decodeTrack :: MonadResource m => ConduitM ByteString (Either Text MCTrack) m ()
decodeTrack = CC.map (\str -> case decode $ fromStrict str of
                                Nothing -> Left ("The following does not appear to be valid JSON track:\n" <> decodeUtf8 str)
                                Just track -> Right track
                     )

sinkTrack' :: MonadResource m => ConduitM (Either Text MCTrack) o m (Either Text MCTrack)
sinkTrack' = do
  mTrack <- await
  case mTrack of
    Nothing -> return $ Left "End of input"
    Just track -> return track

-- dispTrack :: MonadResource m => ConduitM (Either Text Track) o m ()
-- dispTrack = CC.mapM_ consumeTrack

consumeTrack :: (MonadBase IO m, MonadIO m) => TQueue (Track, StationId) -> Either Text MCTrack -> Maybe Track -> StationId -> m (Maybe Track)
consumeTrack _ (Left errMsg) t _ = putStrLn errMsg >> return t
consumeTrack trackQ (Right (MCTrack (track, ttl))) mTrack stationID = do
  newTrack <- case mTrack of
    Nothing -> do
      atomically $ writeTQueue trackQ (track, stationID)
      return track
    Just oldTrack -> case oldTrack == track of
                       True -> return oldTrack
                       False -> do
                            atomically $ writeTQueue trackQ (track, stationID)
                            return track    

  threadDelay $ ttl * 1000
  return $ Just newTrack

baseURL :: Textual t => t
baseURL = "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/"

mainLoop :: (MonadResource m, MonadResourceBase m) => TQueue (Track,StationId) -> Request -> Manager -> StationId -> m ()
mainLoop trackQ request manager stationID = go Nothing
    where
      go mTrack = do
        response <- http request manager
        eTrack <- responseBody response $$+- decodeTrack =$ sinkTrack'

        newTrack <- consumeTrack trackQ eTrack mTrack stationID

        go newTrack

addToDB :: (MonadResource m, MonadResourceBase m) => TQueue (Track,StationId) -> ReaderT SqlBackend m ()
addToDB trackQ = forever $ do
  (track, stationId) <- atomically $ readTQueue trackQ
  entryL <- getTrackEntry track stationId
  case entryL of
    [] -> do
        tId <- insert track
        insert_ $ TrackStations tId stationId 1
    entries -> mapM_ (uncurry updateStationSeen) entries

  transactionSave
  putStrLn $ pprintTrack track

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  q <- newTQueueIO
  _ <- concurrently (
                runSqlite dbLocation $ do
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
weightedChoices = go
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
    runSqlite dbLocation
      $ E.select
      $ E.from $ \(track `E.InnerJoin` station) -> do
          E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
          E.where_ $ station ^. TrackStationsStation E.==. stationId
          return
            ( track ^. TrackArtist
            , track ^. TrackSong
            , station ^. TrackStationsSeen
            )

getTrackEntry
  :: MonadIO m =>
     Track
     -> Key Station
     -> SqlPersistT m [(E.Value (Key Track), E.Value (Key Station))]
getTrackEntry track stationId =
      E.select $
      E.from $ \(trackT `E.InnerJoin` station) -> do
      E.on $ trackT ^. TrackId E.==. station ^. TrackStationsTrack
      E.where_ $ trackT ^. TrackArtist E.==. (E.val $ trackArtist track)
           E.&&. trackT ^. TrackSong E.==. (E.val $ trackSong track)
           E.&&. station ^. TrackStationsStation E.==. (E.val stationId)
      return
        ( station ^. TrackStationsTrack
        , station ^. TrackStationsStation
        )

updateStationSeen
  :: MonadIO m =>
     E.Value (Key Track) -> E.Value (Key Station) -> SqlPersistT m ()
updateStationSeen (E.Value trackKey) (E.Value stationKey) =
    E.update $ \station -> do
      E.set station [TrackStationsSeen E.+=. E.val 1]
      E.where_ (station ^. TrackStationsTrack E.==. (E.val trackKey) E.&&. station ^. TrackStationsStation E.==. (E.val stationKey))

stationTracks :: (MonadIO m, MonadBaseControl IO m) => m [[TrackForStation]]
stationTracks = mapM (\id -> fmap createTrackForStation <$> getTrackForStation (E.val $ StationKey id)) stations

stationCDFs :: [[TrackForStation]] -> [Map Int TrackForStation]
stationCDFs = fmap (cdfMapFromList . fmap f)
    where
      f t@(TrackForStation _ _ n) = (n, t)

stationList :: [(String, Float, Int)]
stationList = [("Hit List", 1, 2)
              , ("Today's Country", 1, 3)
              , ("Solid Gold Oldies", 1, 4)
              , ("Classic Rock", 1, 6)
              , ("Alternative", 1, 14)
              , ("Adult Alternative", 1, 22)
              , ("Classic Country", 1, 27)
              , ("Sounds of the Season", 1, 32)
              , ("Rock Hits", 1, 35)
              , ("70s", 1, 36)
              , ("80s", 1, 38)
              , ("90s", 1, 39)
              , ("Country Hits", 1, 40)
              , ("Rock", 6.35, 44)
              , ("Pop Country", 1, 47)
              , ("Y2k", 1, 48)
              , ("Indie", 1, 117)
              , ("Lounge", 1, 150)
              , ("Radio Paradise", 1, 1000)
              , ("All Things Considered", 1, 1001)
	      ]

readStation
  :: (MonadIO m, MonadBaseControl IO m) =>
     (t, t1, Int) -> m (t1, Map Int TrackForStation)
readStation (_, prob, id) = do
  tracks <- fmap createTrackForStation <$> getTrackForStation (E.val $ StationKey id)
  return (prob, trackProbs tracks)

trackProbs :: [TrackForStation] -> Map Int TrackForStation
trackProbs tracks = cdfMapFromList $ fmap f tracks
    where
      f t@(TrackForStation _ _ n) = (n, t)

stationProbs :: (MonadBaseControl IO m, MonadIO m) => m (Map Float (Map Int TrackForStation))
stationProbs = do
  stations <- filter (\(_, x) -> x /= mempty) <$> mapM readStation stationList
  return $ cdfMapFromList stations

createMix :: Int -> Int -> Int -> IO [TrackForStation]
createMix stations tracks n = do
  stProb <- stationProbs
  let r = fmap concat $ fmap concat $ replicateM n <$> join $ mapM (weightedSampleCDF tracks) <$> weightedSampleCDF stations stProb

  runRVar r DevURandom

dbLocation = "/Users/josh/Library/Application Support/Me/MyMixer/newDB.sqlite"

searchBaseURL = "https://api.spotify.com/v1/search?q="

decodeTrackSearch :: MonadResource m => ConduitM (PositionRange, Value) (Either Text (TrackSearch [] Track)) m ()
decodeTrackSearch = CC.map (\(_, x) -> case fromJSON x of
                                         Error msg -> Left $ pack msg
                                         Success val -> Right val
                           )

getClosestMatch :: Track -> TrackSearch [] Track -> Maybe (Track, Int)
getClosestMatch target (TrackSearch (PagingObject l)) = closestMatch target l

consumeMatch :: MonadResource m => Track -> ConduitM (Either Text (Maybe (Track, Int))) o m ()
consumeMatch target = CC.mapM_ handleIt
    where
      handleIt (Left msg) = print msg
      handleIt (Right match) = handleMatch match
      handleMatch Nothing = print "The search term did not return any results."
      handleMatch (Just (track, diff))
                  | diff == 0 = putStrLn $ "I found your track! " <> pprintTrack track
                  | otherwise = putStrLn $ "This is the closest I could find:\n"
                                     <> pprintTrack track
                                     <> "\nYou searched for\n\n"
                                     <> pprintTrack target

findTrack :: (MonadResource m, MonadResourceBase m) => Manager -> Request -> Track -> m ()
findTrack manager request track = do
  response <- http request manager
  responseBody response $$+- conduitParser json =$ decodeTrackSearch =$ CC.map (fmap (getClosestMatch track)) =$ consumeMatch track

searchRequest :: MonadThrow m => Track -> m Request
searchRequest (Track artist name _) = parseRequest $ unpack $ searchBaseURL <> "artist:" <> artist <> "+track:" <> name <> "&type=track"

instance Diffable Track where
    closestMatch target results = minimumByMay diffMin differenceList
        where
          resultParts = fmap diffParts results
          targetParts = diffParts target
          diffParts (Track artist song _) = [unpack (toLower artist), unpack (toLower song)]
          differenceList = zip results $ fmap (diff targetParts) resultParts
          diffMin (_, a) (_, b) = compare a b

diff :: [String] -> [String] -> Int
diff targetParts resultParts = foldl' (+) 0 diffs
    where
      diffs = fmap diff $ zip targetParts resultParts
      diff (target, result) = editDifference target result