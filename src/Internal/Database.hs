{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Internal.Database where

import ClassyPrelude
import ProjectM36.Client.Simple
import ProjectM36.Client hiding (close, withTransaction)
import ProjectM36.Tupleable
import ProjectM36.Relation
import ProjectM36.Key
import Data.Proxy
import Internal.Types

newtype DatabaseT (m :: * -> *) a = DatabaseT { runDatabaseT_ :: ReaderT DbConn m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

mkDatabaseT :: (DbConn -> m a) -> DatabaseT m a
mkDatabaseT = DatabaseT . ReaderT

runDatabaseT :: DatabaseT m a -> DbConn -> m a
runDatabaseT = runReaderT . runDatabaseT_

runDatabaseTIO :: DatabaseT IO a -> IO (Either DbError a)
runDatabaseTIO f = bracket connect disconnect runF
  where
    connect = simpleConnectProjectM36 (InProcessConnectionInfo (CrashSafePersistence "/tmp/mixer.db") emptyNotificationCallback [])
    disconnect (Left _) = return ()
    disconnect (Right conn) = close conn
    runF (Left exc) = return $ Left exc
    runF (Right conn) = Right <$> runDatabaseT f conn

class Monad m => MonadDatabase (m :: * -> *) where
  runTransaction :: Db a -> m (Either DbError a)

class HasTransaction (m :: * -> *) where
  runTransaction' :: DbConn -> Db a -> m (Either DbError a)

instance HasTransaction IO where
  runTransaction' = withTransaction

instance (Monad m, HasTransaction m) => MonadDatabase (DatabaseT m) where
  runTransaction db = mkDatabaseT $ \conn -> runTransaction' conn db

insert :: (MonadDatabase m, MonadIO m, Traversable t, Tupleable a) => t a -> RelVarName -> m ()
insert l relVarName = do
  res <- runTransaction $ mapM execute $ toInsertExpr l relVarName
  print res

insertTrack :: (MonadDatabase m, MonadIO m) => Track -> m ()
insertTrack track = insert [track] "track"

insertStation :: (MonadDatabase m, MonadIO m) => Station -> m ()
insertStation station = insert [station] "station"

insertTrackStations :: (MonadDatabase m, MonadIO m) => TrackStations -> m ()
insertTrackStations trackStations = insert [trackStations] "track_stations"

retrieve :: (MonadDatabase m, MonadIO m, Tupleable a) => RelationalExprBase () -> m (Either DbError [Either RelationalError a])
retrieve rel = do
  eRes <- runTransaction $ query $ rel -- RelationVariable relVarName ()
  case eRes of
    Left exc -> return $ Left exc
    Right res -> do
      tuples <- liftIO $ ProjectM36.Relation.toList res
      return $ Right $ fmap fromTuple tuples

retrieveTrack :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError Track])
retrieveTrack = retrieve $ RelationVariable "track" ()

retrieveTrackStations :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError TrackStations])
retrieveTrackStations = retrieve $ RelationVariable "track_stations" ()

retrieveStation :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError Station])
retrieveStation = retrieve $ RelationVariable "track_stations" ()

-- retrieveTracksForStation :: (MonadDatabase m, MonadIO m) => StationId -> m (Either DbError [Either RelationalError Track])
-- retrieveTracksForStation

-- dbFun :: (MonadDatabase m, MonadIO m) => Track -> m ()
-- dbFun track = do
--   createSchema
--   insertTrack track
--   retrieveTrack

createSchema :: (MonadDatabase m, MonadIO m) => m ()
createSchema = do
  res <- runTransaction $ mapM execute [ toDefineExpr (Proxy :: Proxy Track) "track"
                                       , databaseContextExprForUniqueKey "track" ["_trackId"]
                                       , toDefineExpr (Proxy :: Proxy Station) "station"
                                       , databaseContextExprForUniqueKey "station" ["_stationId"]
                                       , toDefineExpr (Proxy :: Proxy TrackStations) "track_stations"
                                       , databaseContextExprForUniqueKey "track_stations" ["_track", "_station"]
                                       ]
  print res
