{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mixer where

import Prelude ()
import ClassyPrelude hiding (replicateM, putStr)
import qualified ClassyPrelude as CP
import System.IO (hFlush)
import Control.Monad (replicateM)
import Control.Monad.Trans.Resource

import Data.Random.Shuffle.Weighted
import Data.Random.Source.DevRandom
import Data.Random
import Data.Random.Distribution.Uniform.Exclusive
import Network.HTTP.Conduit
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as CC
import Control.Arrow

import Data.Aeson
import ConfigFile

import Core
import Util

data Confirmation
  = Yes
  | No
  deriving (Show, Eq, Read, Typeable)

newtype TrackForStation = TrackForStation (Track, Int, Key Track)
  deriving (Show, Eq)

putStr :: MonadIO m => Text -> m ()
putStr t = CP.putStr t >> (liftIO $ hFlush stdout)

findTrack :: (MonadResource m) => Manager -> Request -> Track -> Key Track -> (ReaderT SqlBackend m) ()
findTrack manager request track trackId = do
  liftIO $ do
    putStrLn "Searching for:"
    print track
  case trackUri track of
    Nothing -> do
      response <- http request manager
      responseBody response $$+- conduitParser json =$ decodeTrackSearch =$ CC.map (fmap (getClosestMatch track)) =$ consumeMatch track trackId
    Just uri -> putStrLn "This track has already been found and I don't need to search for it!"

decodeTrackSearch :: MonadResource m => ConduitM (PositionRange, Value) (Either Text (TrackSearch [] Track)) m ()
decodeTrackSearch = CC.map (\(_, x) -> case fromJSON x of
                                         Error msg -> Left $ pack msg
                                         Success val -> Right val
                           )

findTracks :: (MonadThrow m, MonadIO m, MonadResource m) => FilePath -> Int -> Int -> Int -> (ReaderT SqlBackend m) ()
findTracks configFilePath x y z = go
    where
      go = do
        mgr <- liftIO $ newManager tlsManagerSettings
        tracks <- liftIO $ createMix configFilePath x y z
        mapM_ (f mgr) tracks

        uris <- getTracksFromIds $ trackIds tracks
        -- mapM_ (\(E.Value mUri) -> dispUri mUri) uris
        writeFile "/tmp/results.txt" $ intercalate "\n" $ fmap (\(E.Value mUri) -> dispUri mUri) uris

      f :: (MonadThrow m, MonadIO m, MonadResource m) => Manager -> TrackForStation -> (ReaderT SqlBackend m) ()
      f mgr (TrackForStation (track, _, trackId)) = do
        eReq <- runExceptionT $ searchRequest track
        case eReq of
          Left msg -> liftIO $ print msg
          Right req -> findTrack mgr req track trackId

      trackIds = fmap (\(TrackForStation (_, _, trackId)) -> trackId)

      dispUri Nothing = mempty
      dispUri (Just uri) = tshow uri

getClosestMatch :: Track -> TrackSearch [] Track -> Maybe (Track, Int)
getClosestMatch target (TrackSearch (PagingObject l)) = closestMatch target l

instance Diffable Track where
    closestMatch target results = minimumByMay diffMin differenceList
        where
          resultParts = fmap diffParts results
          targetParts = diffParts target
          diffParts (Track artist song _ _) = [unpack (toLower artist), unpack (toLower song)]
          differenceList = zip results $ fmap (diff targetParts) resultParts
          diffMin (_, a) (_, b) = compare a b

consumeMatch :: MonadResource m => Track -> Key Track -> ConduitM (Either Text (Maybe (Track, Int))) o (ReaderT SqlBackend m) ()
consumeMatch target trackId = CC.mapM_ handleIt
    where
      handleIt (Left msg) = print msg
      handleIt (Right match) = handleMatch match
      handleMatch Nothing = do
        putStrLn $ "Your search for:\n" <> pprintTrack target <> "\n did not return any results."
        askPaste
      handleMatch (Just (track, diff))
                  | diff == 0 = updateUri (trackUri track)
                  | otherwise = do
                       putStrLn $ "This is the closest I could find:\n"
                                     <> pprintTrack track
                                     <> "\nYou searched for\n\n"
                                     <> pprintTrack target
                       putStr "Is this correct? (Y/N): "
                       yesOrNo (updateUri (trackUri track)) askPaste
      updateUri :: MonadIO m => Maybe Text -> SqlPersistT m ()
      updateUri uri = updateTrackUri trackId uri
      askPaste :: (MonadIO m) => SqlPersistT m ()
      askPaste = liftIO (putStr "Would you like to paste the URI here? (Y/N): ") >> yesOrNo pasteUri (return ())
      pasteUri :: MonadIO m => SqlPersistT m ()
      pasteUri = do
        pastedUri <- liftIO getLine
        putStrLn pastedUri
        updateUri (Just pastedUri)

      yesOrNo f g = do
        -- answer <- liftBase $ option Yes "For Yes"
        -- case answer of
        --   Yes -> f
        --   No -> g
        (answer :: Text) <- getLine
        case answer of
          "Y" -> f
          "N" -> g
          _ -> putStrLn "I don't understand that answer. Please enter only Y/N" >> yesOrNo f g

createMix :: FilePath -> Int -> Int -> Int -> IO [TrackForStation]
createMix configFilePath stations tracks n = go
 where
   go = do
     stProb <- stationProbs configFilePath
     -- let r = fmap concat $ fmap concat $ repeatedSampling stProb
     let r = fmap concat $ fmap concat $ replicateM n <$> join $ mapM (weightedSampleCDF tracks) <$> weightedSampleCDF stations stProb
  
     runRVar r DevURandom
   -- repeatedSampling :: Map Float (Map Int TrackForStation) -> RVarT Identity [[[TrackForStation]]]
   -- repeatedSampling stProb = replicateM n <$> weightedList stProb
   -- weightedList :: Map Float (Map Int TrackForStation) -> RVarT Identity [[TrackForStation]]
   -- weightedList stProb = join $ mapM (weightedSampleCDF tracks) <$> weightedSampleCDF stations stProb

stationProbs :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => FilePath -> m (Map Float (Map Int TrackForStation))
stationProbs configFilePath = do
  stationList <- runKleisli (setStationWeights >>> arr (fmap toStationTriple)) configFilePath
  stations <- filter (\(_, x) -> x /= mempty) <$> mapM readStation stationList
  return $ cdfMapFromList stations

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


-- stationList :: [(String, Float, Int)]
-- stationList = [("Hit List", defProb, 2)
--               , ("Today's Country", defProb, 3)
--               , ("Solid Gold Oldies", defProb, 4)
--               , ("Classic Rock", 7.5, 6)
--               , ("Alternative", 7.5, 14)
--               , ("Adult Alternative", 7.5, 22)
--               , ("Classic Country", defProb, 27)
--               , ("Sounds of the Season", defProb, 32)
--               , ("Rock Hits", defProb, 35)
--               , ("70s", defProb, 36)
--               , ("80s", defProb, 38)
--               , ("90s", defProb, 39)
--               , ("Country Hits", defProb, 40)
--               , ("Rock", 25, 44)
--               , ("Pop Country", defProb, 47)
--               , ("Y2k", defProb, 48)
--               , ("Indie", defProb, 117)
--               , ("Lounge", 7.5, 150)
--               , ("Radio Paradise", 7.5, 1000)
--               , ("All Things Considered", 7.5, 1001)
--               ]
--   where
--     defProb = 2.8125

-- stationList :: [(String, Float, Int)]
-- stationList = [("Hit List", 10, 2)
--               , ("Today's Country", 10, 3)
--               , ("Solid Gold Oldies", 10, 4)
--               , ("Adult Alternative", defProb, 22)
--               , ("Classic Country", defProb, 27)
--               , ("Sounds of the Season", 20, 32)
--               , ("70s", 10, 36)
--               , ("80s", defProb, 38)
--               , ("90s", defProb, 39)
--               , ("Country Hits", defProb, 40)
--               , ("Pop Country", defProb, 47)
--               , ("Radio Paradise", defProb, 1000)
--               , ("Soft Rock", 10, 1)
--               , ("Singers & Swing", defProb, 18)
--               , ("Rock", defProb, 44)
--               ]
--   where
--     defProb = 3.33

readStation
  :: (MonadIO m, MonadBaseControl IO m) =>
     (t, t1, Int) -> m (t1, Map Int TrackForStation)
readStation (_, prob, id) = do
  tracks <- fmap createTrackForStation <$> getTrackForStation (E.val $ StationKey id)
  return (prob, trackProbs tracks)

createTrackForStation :: (E.Value Text, E.Value Text, E.Value (Maybe Text), E.Value (Maybe Text), E.Value Int, E.Value (Key Track)) -> TrackForStation
createTrackForStation (E.Value artist, E.Value song, E.Value album, E.Value uri, E.Value seen, E.Value trackId) = TrackForStation (Track artist song album uri, seen, trackId)

searchBaseURL = "https://api.spotify.com/v1/search?q="

getTracksFromIds l =
    E.select $
    E.from $ \track -> do
      E.where_ (E.in_ (track ^. TrackId) (E.valList l))
      return ( track ^. TrackUri )

searchRequest :: MonadThrow m => Track -> m Request
searchRequest (Track artist name _ _) = parseRequest $ unpack $ searchBaseURL <> "artist:" <> artist <> "+track:" <> name <> "&type=track"

updateTrackUri :: MonadIO m => Key Track -> Maybe Text -> SqlPersistT m ()
updateTrackUri trackKey trackUri =
    E.update $ \track -> do
      E.set track [TrackUri E.=. (E.val trackUri)]
      E.where_ (track ^. TrackId E.==. (E.val trackKey))

getTrackForStation
  :: (MonadIO m, MonadBaseControl IO m)
     => E.SqlExpr (E.Value (Key Station))
     -> m [(E.Value Text, E.Value Text, E.Value (Maybe Text), E.Value (Maybe Text), E.Value Int, E.Value (Key Track))]
getTrackForStation stationId =
    runSqlite dbLocation
      $ E.select
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

trackProbs :: [TrackForStation] -> Map Int TrackForStation
trackProbs tracks = cdfMapFromList $ fmap f tracks
    where
      f t@(TrackForStation (_, n, _)) = (n, t)

