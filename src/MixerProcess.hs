{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}

module MixerProcess where

import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Data.Random.Shuffle.Weighted
import Data.Random hiding (cdf)
import Data.Random.Source.DevRandom
import Data.Random.Distribution.Uniform.Exclusive (Excludable)
import Control.Monad.Trans.Resource hiding (throwM)
import Data.Aeson

import Network.HTTP.Conduit

import MachineUtils
import ParseCSV
import Core
import ConfigFile

import ClassyPrelude

import Util

data StationTracks_ a = StationTracks_ (Key Station) a
  deriving Show

type StationTracks = StationTracks_ [TrackForStation]
type TrackProbs = Map Int [(Text, Text)]

instance Functor StationTracks_ where
  fmap f (StationTracks_ k tracks) = StationTracks_ k (f tracks)

data TrackForStation = TrackForStation
  { trackForStationArtist :: E.Value Text
  , trackForStationSong :: E.Value Text
  , trackForStationAlbum :: E.Value (Maybe Text)
  , trackForStationUri :: E.Value (Maybe Text)
  , trackForStationSeen :: E.Value Int
  , trackForStationTrackId :: E.Value (Key Track)
  } deriving Show

extractIt :: TrackForStation -> (Int, Track)
extractIt TrackForStation { trackForStationArtist = (E.Value artist)
                          , trackForStationSeen = (E.Value seen)
                          , trackForStationSong = (E.Value song)
                          , trackForStationAlbum = (E.Value album)
                          , trackForStationUri = (E.Value uri)
                          } = (seen, Track artist song album uri)

getTrackForStation :: MonadIO m => ProcessA (Kleisli (ReaderT E.SqlBackend m)) (Event (Key Station)) (Event StationTracks)
getTrackForStation = machine getTrackForStation'

getTrackForStation' :: MonadIO m => Key Station -> ReaderT E.SqlBackend m StationTracks
getTrackForStation' stationId = do
  tracks <- getTrackForStation_ (E.val stationId)
  return $ StationTracks_ stationId $ fmap mkTrackForStation tracks
  where
    mkTrackForStation (artist, song, album, uri, seen, id) =
      TrackForStation artist song album uri seen id

getTrackForStation_ :: MonadIO m => E.SqlExpr (E.Value (Key Station)) -> E.SqlPersistT m [(E.Value Text, E.Value Text, E.Value (Maybe Text), E.Value (Maybe Text), E.Value Int, E.Value (Key Track))]
getTrackForStation_ stationId = E.select
  $ E.from $ \(track `E.InnerJoin` station) -> do
      E.on $ track ^. TrackId E.==. station ^. TrackStationsTrack
      E.where_ $ station ^. TrackStationsStation E.==. stationId
      return
            ( track ^. TrackArtist
            , track ^. TrackSong
            , track ^. TrackAlbum
            , track ^. TrackUri
            , station ^. TrackStationsSeen
            , track ^. TrackId
            )

sampleItem :: (MonadRandom b, Distribution Uniform w, Excludable w, MonadBase b m, Ord w, Num w) => Int -> Kleisli m (Map w a) [a]
sampleItem n = Kleisli (liftBase . sample . weightedSampleCDF n)

process :: ArrowApply a => (b -> c) -> ProcessA a (Event b) (Event c)
process = anytime . arr

scanl :: (IsSequence t, SemiSequence seq, Monoid seq) => (Element seq -> Element t -> Element seq) -> Element seq -> t -> seq
scanl = scanlGo
  where
    scanlGo f q ls = q `cons` (case headMay ls of
                                 Nothing -> mempty
                                 Just x -> scanlGo f (f q x) (tailEx ls)
                         )

scanl1 :: IsSequence t => (Element t -> Element t -> Element t) -> t -> t
scanl1 f xs = case headMay xs of
  Nothing -> xs
  Just x -> scanl f x xs

cdf :: (Element t ~ (t2, t1), IsSequence t, Num t2) => t -> t
cdf = scanl1 (\(p1, x1) (p2, x2) -> (p1 + p2, x2))

cdfa
  :: (Element c ~ (t1, t), Arrow a, IsSequence c, Num t1) => a c c
cdfa = arr cdf

cdfp :: (Element c ~ (t1, t), ArrowApply a, IsSequence c, Num t1) =>
     ProcessA a (Event c) (Event c)
cdfp = anytime cdfa

searchTracks
  :: MonadResource m => Manager -> Int -> Int -> ProcessA (Kleisli m) (Event Track) (Event Track, Event (TrackSearch [] Track))
searchTracks mgr nStations nTracks = proc track -> do
  searchResult <- machine searchRequest >>> evMap (\req -> (req, mgr)) >>> sourceHttp >>> machineParser' json >>> machine fromJSON' >>> evMap asTrackSearch -< track
  returnA -< (track, searchResult)

createMix :: (MonadResource m, Distribution Uniform w, Excludable w, Ord w, Num w) =>
             Manager -> Int -> Int
          -> ProcessA
          (Kleisli (ReaderT E.SqlBackend m))
          (Event (Map w (Key Station)))
          (Event SearchResult)
createMix mgr nStations nTracks = proc input -> do
  storedTrack <- anytime (sampleItem nStations) >>> blockingSource' >>> getTrackForStation >>> evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap mapFromList >>> anytime (sampleItem nTracks) >>> blockingSource' -< input
  mTrack <- evMap Just >>> hold Nothing -< storedTrack
  case mTrack of
    Nothing -> returnA -< noEvent
    Just track -> do
      mUri <- arr trackUri -< track
      case mUri of
        Just _ -> evMap Cached -< storedTrack
        Nothing -> do
          result <- searchTracks mgr nStations nTracks >>> mergeEvents >>> evMap (uncurry getClosestMatch) -< storedTrack
          searchResult <- mergeEvents >>> evMap (uncurry handleResult) -< (track <$ storedTrack, result)
          returnA -< searchResult
  where
    handleResult track Nothing = NotFound track
    handleResult track (Just (foundTrack, diff))
      | diff == 0 = ExactMatch foundTrack
      | otherwise = ClosestMatch track foundTrack

searchBaseURL = "https://api.spotify.com/v1/search?q="

searchRequest :: MonadThrow m => Track -> m Request
searchRequest (Track artist name _ _) = parseRequest $ unpack $ searchBaseURL <> "artist:" <> artist <> "+track:" <> name <> "&type=track"

fromJSON' :: (MonadThrow m, FromJSON a) => Value -> m a 
fromJSON' v = case fromJSON v of
  Error e -> throwM $ ParseError e
  Success x -> return x

asTrackSearch :: TrackSearch [] Track -> TrackSearch [] Track
asTrackSearch = id

readStationWeight :: MonadThrow m => Config -> m [StationWeight]
readStationWeight cfg = getVal "weights" cfg

setDefProb :: [StationWeight] -> Float -> [StationWeight]
setDefProb weights defProb = fmap setIt weights
  where
    setIt station@(StationWeight {stationProb = DefProb}) = station {stationProb = Prob defProb }
    setIt station = station

calculateDef :: [StationWeight] -> Float
calculateDef weights = (100 - totalProb) / (fromIntegral nDefs)
  where
    addIt (totalProb, nDefs) DefProb = (totalProb, nDefs + 1)
    addIt (totalProb, nDefs) (Prob p) = (totalProb + p, nDefs)
    (totalProb, nDefs) = foldl' addIt (0, 0) $ fmap stationProb weights

setStationWeights :: (MonadIO m, MonadThrow m) => Kleisli m FilePath [StationWeight]
setStationWeights = Kleisli readConfigFile >>> Kleisli readStationWeight >>> (id &&& arr calculateDef) >>> arr (uncurry setDefProb)

toStationTriple :: StationWeight -> (String, Float, Int)
toStationTriple (StationWeight (StationName_ n) (Prob p) (StationID id)) = (unpack n, p, id)

toStationTuple :: StationWeight -> (Float, Key Station)
toStationTuple (StationWeight _ (Prob p) (StationID id)) = (p, StationKey id)

toStationCDF :: ArrowApply a =>
     ProcessA
       a (Event [StationWeight]) (Event (Map Float (Key Station)))
toStationCDF = evMap (fmap toStationTuple) >>> cdfp >>> evMap mapFromList >>> evMap asMap

getClosestMatch :: Track -> TrackSearch [] Track -> Maybe (FoundTrack, Int)
getClosestMatch target (TrackSearch (PagingObject l)) = do
  (t, d) <- closestMatch target l
  return (FoundTrack t, d)

instance Diffable Track where
    closestMatch target results = minimumByMay diffMin differenceList
        where
          resultParts = fmap diffParts results
          targetParts = diffParts target
          diffParts (Track artist song _ _) = [unpack (toLower artist), unpack (toLower song)]
          differenceList = zip results $ fmap (diff targetParts) resultParts
          diffMin (_, a) (_, b) = compare a b

newtype FoundTrack = FoundTrack Track
  deriving Show

data SearchResult
  = Cached Track
  | ExactMatch FoundTrack
  | ClosestMatch Track FoundTrack
  | NotFound Track
  deriving Show

-- runIt = runSqlite "/tmp/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF >>> createMix mgr 5 2 >>> machine print) ["/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"]
