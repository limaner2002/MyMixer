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
import Transient.Base
import Control.Monad.Base

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as CC

import Data.Aeson

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

findTracks :: (MonadThrow m, MonadIO m, MonadResource m) => (ReaderT SqlBackend m) ()
findTracks = go
    where
      go = do
        mgr <- liftIO $ newManager tlsManagerSettings
        tracks <- liftIO $ createMix 5 2 10
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

createMix :: Int -> Int -> Int -> IO [TrackForStation]
createMix stations tracks n = go
 where
   go = do
     stProb <- stationProbs
     -- let r = fmap concat $ fmap concat $ repeatedSampling stProb
     let r = fmap concat $ fmap concat $ replicateM n <$> join $ mapM (weightedSampleCDF tracks) <$> weightedSampleCDF stations stProb
  
     runRVar r DevURandom
   -- repeatedSampling :: Map Float (Map Int TrackForStation) -> RVarT Identity [[[TrackForStation]]]
   -- repeatedSampling stProb = replicateM n <$> weightedList stProb
   -- weightedList :: Map Float (Map Int TrackForStation) -> RVarT Identity [[TrackForStation]]
   -- weightedList stProb = join $ mapM (weightedSampleCDF tracks) <$> weightedSampleCDF stations stProb

stationProbs :: (MonadBaseControl IO m, MonadIO m) => m (Map Float (Map Int TrackForStation))
stationProbs = do
  stations <- filter (\(_, x) -> x /= mempty) <$> mapM readStation stationList
  return $ cdfMapFromList stations

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

createTrackForStation :: (E.Value Text, E.Value Text, E.Value Text, E.Value (Maybe Text), E.Value Int, E.Value (Key Track)) -> TrackForStation
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
     -> m [(E.Value Text, E.Value Text, E.Value Text, E.Value (Maybe Text), E.Value Int, E.Value (Key Track))]
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
