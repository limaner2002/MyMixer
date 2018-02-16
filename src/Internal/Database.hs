{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Internal.Database where

import ClassyPrelude
import ProjectM36.Client.Simple
import ProjectM36.Client hiding (close, withTransaction)
import ProjectM36.Tupleable
import ProjectM36.Relation
import ProjectM36.Key
import ProjectM36.Base
import Data.Proxy
import Internal.Types
import Control.Monad.Trans.Compose
import Control.Monad.Except

infixr 9 :.:
type (:.:) = ComposeT

newtype DatabaseET err (m :: * -> *) a = DatabaseET { runDatabaseET :: (ExceptT err :.: DatabaseT) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

mkDatabaseET :: DatabaseT m (Either err a) -> DatabaseET err m a
mkDatabaseET = DatabaseET . ComposeT . ExceptT

newtype DatabaseT (m :: * -> *) a = DatabaseT { runDatabaseT_ :: ReaderT DbConn m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

mkDatabaseT :: (DbConn -> m a) -> DatabaseT m a
mkDatabaseT = DatabaseT . ReaderT

runDatabaseT :: DatabaseT m a -> DbConn -> m a
runDatabaseT = runReaderT . runDatabaseT_

runDatabaseETIO :: DatabaseET DbError IO a -> IO (Either DbError a)
runDatabaseETIO = fmap ClassyPrelude.join . runDatabaseTIO . runExceptT . getComposeT . runDatabaseET

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

retrieveE :: (Tupleable a, MonadIO m, HasTransaction m) => RelationalExprBase () -> DatabaseET DbError m [Either RelationalError a]
retrieveE = mkDatabaseET . retrieveAll

retrieveAll :: (MonadDatabase m, Tupleable a)
   => RelationalExprBase () -> m (Either DbError [Either RelationalError a])
retrieveAll = retrieveAll_

retrieveAll_ :: (Element (f RelationTuple) ~ RelationTuple, MonadDatabase m,
                 SemiSequence (f RelationTuple), Monoid (f RelationTuple),
                 Tupleable a, Functor f) =>
                RelationalExpr -> m (Either DbError (f (Either RelationalError a)))
retrieveAll_ rel = do
  eRes <- runTransaction $ query $ rel
  case eRes of
    Left exc -> return $ Left exc
    Right res -> do
      let tuples = relFold cons mempty res
      return $ Right $ fmap fromTuple tuples

retrieveTrack :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError Track])
retrieveTrack = retrieveAll $ RelationVariable "track" ()

retrieveTrackStations :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError TrackStations])
retrieveTrackStations = retrieveAll $ RelationVariable "track_stations" ()

retrieveStation :: (MonadDatabase m, MonadIO m) => m (Either DbError [Either RelationalError Station])
retrieveStation = retrieveAll $ RelationVariable "track_stations" ()

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
