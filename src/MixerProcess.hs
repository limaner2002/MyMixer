{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Network.HTTP.Types as HT

import MachineUtils
import ParseCSV
import Core
import ConfigFile

import ClassyPrelude

import Util
import WeightedMap
import WeightedShuffle hiding (sampleItem, sample)

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

extractIt :: TrackForStation -> (Int, (Key Track, Track))
extractIt TrackForStation { trackForStationArtist = (E.Value artist)
                          , trackForStationSeen = (E.Value seen)
                          , trackForStationSong = (E.Value song)
                          , trackForStationAlbum = (E.Value album)
                          , trackForStationUri = (E.Value uri)
                          , trackForStationTrackId = (E.Value k)
                          } = (seen, (k, Track artist song album uri))

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
      E.where_ $ do
        station ^. TrackStationsStation E.==. stationId
        E.&&. station ^. TrackStationsSeen E.>. (E.val 0)
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
  :: (MonadResource m, MonadThrow e) => Manager -> ProcessA (Kleisli m) (Event Track) (Event (e (Track, TrackSearch [] Track)))
searchTracks mgr = proc trackEvt -> do
  mTrack <- evMap Just >>> hold Nothing -< trackEvt
  case mTrack of
    Nothing -> returnA -< noEvent
    Just track -> do
      searchResultBS <- machine searchRequest >>> evMap (\req -> (req, mgr)) >>> sourceHttp -< trackEvt
      searchResult <- machineParser json >>> evMap (join . fmap fromJSON') >>> evMap (fmap asTrackSearch) -< searchResultBS
      returnA -< fmap (\result -> (,) <$> pure track <*> result) searchResult -- (track, searchResult)

data SampledTrack a = SampledTrack
  { stationKey :: Key Station
  , trackKey :: Key Track
  , searchResult :: SearchResult a
  } deriving Show

createMix :: (MonadResource m, MonadThrow e) =>
             Manager -> Int -> Int
          -> ProcessA
          (Kleisli (ReaderT E.SqlBackend m))
          (Event (Float, WeightMap Float (Key Station)))
          (Event (e (SampledTrack Unconfirmed)))
createMix mgr nStations nTracks = proc input -> do
  -- storedTrack <- anytime (sampleItem nStations) >>> blockingSource' >>> getTrackForStation >>> evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap mapFromList >>> anytime (sampleItem nTracks) >>> blockingSource' -< input
  -- sampledStation <- anytime (sampleItem nStations) >>> blockingSource' -< input
  sampledStation <- weightedSamplesRemove nStations >>> evMap (fmap snd) -< input
  sampledTrack <- id >>? getTrackForStation >>> createTrackMap >>> sampleTracks -< sampledStation
  mTrack <- hold Nothing -< sampledTrack
  mStation <- hold Nothing -< sampledStation
  case mStation of
    Nothing -> returnA -< noEvent
    Just station ->
      case mTrack of
        Nothing -> returnA -< noEvent
        Just (k, track) -> do
          mUri <- arr trackUri -< track
          case mUri of
            Just _ -> evMap pure -< SampledTrack station k (Cached track) <$ sampledTrack
            Nothing -> do
              -- result <- searchTracks mgr >>> evMap (fmap $ uncurry getClosestMatch)) -< track <$ input
              result <- searchTracks mgr >>> evMap (fmap $ uncurry getClosestMatch) -< track <$ sampledTrack
              -- searchResult <- mergeEvents >>> evMap (fmap $ uncurry handleResult) -< (track <$ storedTrack, result)
              searchResult <- evMap (fmap $ uncurry handleResult) -< fmap (\r -> (,) <$> pure track <*> r) result
              -- output <- mergeEvents >>> evMap (\(k, res) -> (,) <$> pure k <*> res) -< (k, result)
              returnA -< fmap (\r -> SampledTrack <$> pure station <*> pure k <*> r) searchResult
  where
    handleResult track Nothing = NotFound track
    handleResult track (Just (foundTrack, diff))
      | diff == 0 = ExactMatch foundTrack
      | otherwise = ClosestMatch track foundTrack
    sampleTracks = weightedSamplesRemove nTracks >>> evMap (fmap snd)
    -- sampleStations = weightedSamplesRemove nStations >>> evMap (fmap snd)

searchBaseURL = "https://api.spotify.com/v1/search?q="

searchRequest :: MonadThrow m => Track -> m Request
searchRequest (Track artist name _ _) = parseRequest $ unpack $ searchBaseURL <> "artist:" <> urlEncode artist <> "+track:" <> urlEncode name <> "&type=track"
  where
    urlEncode = decodeUtf8 . HT.urlEncode False . encodeUtf8

fromJSON' :: (MonadThrow m, FromJSON a) => Value -> m a 
fromJSON' v = case fromJSON v of
  Error e -> throwM $ ParseError (e <> ": " <> (unpack . decodeUtf8 . encode $ v))
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

                                                           -- Very similar to createTrackMap
toStationCDF :: ArrowApply a =>
     ProcessA
       a (Event [StationWeight]) (Event (Float, WeightMap Float (Key Station)))
toStationCDF = evMap (fmap toStationTuple) >>> cdfp >>> evMap (fromMaybe 0 . lastMay . fmap fst) &&& evMap mapFromList >>> mergeEvents

toStationCDF' :: MonadIO m =>
                 ProcessA
                 (Kleisli m)
                 (Event [StationWeight])
                 (Event (Float, WeightMap Float (Key Station)))
toStationCDF' = evMap (fmap toStationTuple) >>> cdfp >>> evMap (maximumEx . fmap fst) &&& evMap mapFromList >>> mergeEvents

getClosestMatch :: Track -> TrackSearch [] Track -> Maybe (FoundTrack, Int)
getClosestMatch target (TrackSearch (PagingObject l)) = go
  where
    go = do
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

data Unconfirmed = Unconfirmed
data Confirmed = Confirmed

data SearchResult a
  = Cached Track
  | ExactMatch FoundTrack
  | ClosestMatch Track FoundTrack
  | UserPasted Text
  | NotFound Track
  deriving Show

type TrackWeights = (Int, WeightMap Int (Key Track, Track))
type StationWeights = (Float, WeightMap Int (Key Station))

cacheStation :: MonadIO m =>
     ProcessA
       (Kleisli (ReaderT E.SqlBackend m))
       (Event (Key Station), Event (Map (Key Station) TrackWeights))
       (Event TrackWeights, Event (Map (Key Station) TrackWeights))
cacheStation = simpleCache ((getTrackForStation >>> createTrackMap) >>| id >>> evMap Just)

               -- Very similar to toStationCDF
createTrackMap :: ArrowApply a => ProcessA a (Event StationTracks) (Event TrackWeights)
createTrackMap = evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap (fromMaybe 0 . lastMay . fmap fst) &&& evMap mapFromList >>> mergeEvents

-- sampleTracks :: ArrowApply a => ProcessA a (Event TrackWeights, Event TrackWeights) (Event Track, Event TrackWeights)
-- sampleTracks = proc (input, feedback) -> do
  
-- decrementSeen :: Arrow cat => cat (Int, Track) (Maybe (Int, Track))
-- decrementSeen = arr dec
--   where
--     dec (0, _) = Nothing
--     dec (n, track) = Just (n - 1, track)

                 -- This needs to take in the station key as well in order to properly decrement the counter.
decrementSeen :: MonadIO m => SampledTrack Confirmed -> E.SqlPersistT m ()
decrementSeen SampledTrack {trackKey = trackKey, stationKey = stationKey} = do
  liftIO $ putStrLn "Decrementing seen counter."
  E.update $ \station -> do
      E.set station [TrackStationsSeen E.-=. (E.val 1)]
      E.where_ ( station ^. TrackStationsTrack E.==. (E.val trackKey)
           E.&&. station ^. TrackStationsSeen E.>=. (E.val 1)
           E.&&. station ^. TrackStationsStation E.==. (E.val stationKey)
               )

updateURI :: MonadIO m => SampledTrack Confirmed -> E.SqlPersistT m ()
updateURI SampledTrack {trackKey = trackKey, searchResult = res} =
  case resultURI res of
    Nothing -> return ()
    Just uri -> updateURI_ trackKey uri

updateURI_ :: MonadIO m => Key Track -> Text -> E.SqlPersistT m ()
updateURI_ trackKey uri = do
  liftIO $ putStrLn "Updating track URI."
  E.update $ \track -> do
    E.set track [TrackUri E.=. E.val (Just uri)]
    E.where_ ( track ^. TrackId E.==. (E.val trackKey))

-- runIt = runSqlite "/Users/josh/Google Drive/MyMixer/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF >>> createMix mgr 5 2 >>> machine print) ["/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"]
-- runIt = runSqlite "/Users/josh/Google Drive/MyMixer/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF' >>> weightedSamplesRemove 5 >>> anytime (Kleisli asMonadThrow) >>> getTrackForStation >>> evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap (maximumEx . fmap fst) &&& (evMap mapFromList >>> evMap asWM) >>> mergeEvents >>> weightedSamplesRemove 2 >>> anytime (Kleisli asMonadThrow) >>> machine print) ["/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"]
-- runIt = runSqlite "/Users/josh/Google Drive/MyMixer/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF' >>> weightedSamplesRemove 5 >>> anytime (Kleisli asMonadThrow) >>> getTrackForStation >>> machine print) ["/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"]

              -- With caching
-- runIt = runSqlite "/Users/josh/Google Drive/MyMixer/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF' >>> weightedSamplesRemove 5 >>> anytime (Kleisli asMonadThrow) >>> cacheTracks >>> evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap (maximumEx . fmap fst) &&& (evMap mapFromList >>> evMap asWM) >>> mergeEvents >>> weightedSamplesRemove 2 >>> anytime (Kleisli asMonadThrow) >>> machine print) ["/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"]

-- let asWM = id :: WeightMap Int Track -> WeightMap Int Track
-- runSqlite "/tmp/newDB.sqlite" $ runKleisli (run_ $ anytime setStationWeights >>> toStationCDF' >>> weightedSamplesRemove 5 >>? loop cacheTracks >>> evMap (\(StationTracks_ _ ts) -> fmap extractIt ts) >>> cdfp >>> evMap (fromMaybe 0 . lastMay . fmap fst) &&& (evMap mapFromList >>> evMap asWM) >>> mergeEvents >>> weightedSamplesRemove 2 >>? machine print) $ take 2 $ repeat "/Users/josh/workspace/MyMixer/stationWeights/stationWeights.cfg"

t :: Track
t = Track {trackArtist = "The Commodores", trackSong = "Sweet Love", trackAlbum = Just "All The Great Love Songs (1976)", trackUri = Nothing}

asTrackWeight :: WeightMap Int (Key Track, Track) -> WeightMap Int (Key Track, Track)
asTrackWeight = id

asStationWeight :: WeightMap Int (Key Station) -> WeightMap Int (Key Station)
asStationWeight = id

-- let updateIt = machine (mapM decrementSeen) &&& (evMap (fmap searchResult) >>> machine (mapM (liftIO . userInput)) >>> evMap (fmap resultURI) >>> machine print >>| (id >>? evMap (<> "\n") >>> sinkFile "/tmp/result.txt")) >>> tee
updateIt
  :: MonadResource m => ProcessA (Kleisli (ReaderT E.SqlBackend m)) (Event (Either SomeException (SampledTrack Unconfirmed))) (Event ())
updateIt = proc input -> do
  confirmed <- machine (mapM (liftIO . confirmSearchResult)) -< input
  decSeen <- machine (mapM decrementSeen) >>> machine print >>| id -< confirmed
  updURI <- machine (mapM updateURI) >>> machine print >>| id -< confirmed
  toFile <- evMap (fmap (resultURI . searchResult)) >>> machine print >>| (id >>? evMap (<> "\n") >>> sinkFile "/tmp/result.txt") -< confirmed
  evts <- gather -< [decSeen, updURI, toFile]
  returnA -< evts

resultURI :: SearchResult Confirmed -> Maybe Text
resultURI (Cached t) = trackUri t
resultURI (ExactMatch (FoundTrack t)) = trackUri t
resultURI (UserPasted uri) = Just uri
resultURI _ = Nothing

confirmSearchResult :: MonadIO m => SampledTrack Unconfirmed -> m (SampledTrack Confirmed)
confirmSearchResult track@(SampledTrack {searchResult = result}) = do
  confirmed <- liftIO $ confirmSearchResult_ result
  return $ track {searchResult = confirmed}

confirmSearchResult_ :: SearchResult Unconfirmed -> IO (SearchResult Confirmed)
confirmSearchResult_ (ClosestMatch target found@(FoundTrack foundT)) = do
  putStrLn $ "You searched for\n\n"
          <> pprintTrack target
          <> "\n\nand this is the closest I could find\n"
          <> pprintTrack foundT
  putStr "Is this correct? (Y/N): "
  confirm <- yesOrNo (return $ ExactMatch found) askPaste
  case confirm of
    Yes good -> pure good
    No bad -> pure bad
    where
      askPaste = do
        confirm <- putStr "Would you like to paste the URI here? (Y/N): " >> yesOrNo (UserPasted <$> getLine) (pure $ NotFound target)
        case confirm of
          Yes pasted -> pure pasted
          No bad -> pure bad

confirmSearchResult_ (NotFound target) = do
  putStrLn $ "Your search for:\n" <> pprintTrack target <> "\n did not return any results."
  putStr "Would you like to paste the URI? "
  confirm <- yesOrNo (UserPasted <$> getLine) (pure $ NotFound target)
  case confirm of
    Yes good -> pure good
    No bad -> pure bad
confirmSearchResult_ (ExactMatch t) = pure (ExactMatch t)
confirmSearchResult_ (Cached t) = pure (Cached t)
confirmSearchResult_ (UserPasted uri) = pure (UserPasted uri)


data Confirmation a b
  = Yes a
  | No b
  deriving (Show, Eq, Read)

yesOrNo :: IO a -> IO b -> IO (Confirmation a b)
yesOrNo f g = do
  (answer :: Text) <- getLine
  case answer of
    "Y" -> Yes <$> f
    "N" -> No <$> g
    _   -> putStrLn "I don't understand that answer. Please enter only Y/N" >> yesOrNo f g

