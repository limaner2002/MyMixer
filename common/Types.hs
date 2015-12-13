{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types ( Data.Aeson.decode
             , Track (..)
             , Saveable (..)
             , SpotifyAlbumEntry (..)
             , SpotifyArtistEntry (..)
             , SpotifyTrackEntry (..)
             , spotifyTrackEntryKey
             , spotifyAlbumEntryKey
             , spotifyArtistEntryKey
             , Database.Persist.Sqlite.runSqlite
             , Database.Persist.Sql.runMigration
             , Database.Persist.Sql.transactionSave
             , Database.Persist.Sql.SqlBackend
             , Database.Persist.Sql.toSqlKey
             , Database.Persist.Sql.fromSqlKey
             , Database.Persist.Key
             , Database.Persist.insert
             , Database.Persist.insert_
             , Database.Persist.repsert
             , insertNotExists
             , migrateAll
             , saveTrack
             , getStations
             , renderStations
             , Control.Monad.State.evalStateT
             , Control.Monad.State.get
             , Control.Monad.State.put
             , Scraper
             , RPScraper
             , SourcePlaylists
             , sourcePlaylistsUuid
             , getScrapedTracks
             , getSourcePlaylists
             , Control.Monad.IO.Class.liftIO
             , Control.Monad.IO.Class.MonadIO
             , Control.Monad.Trans.Resource.MonadResource
             , Control.Monad.Reader.ReaderT
             )where

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist
import Database.Persist.Quasi
import Data.Aeson
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger (NoLoggingT, LoggingT, logInfoN)
import qualified Data.Text as T

import Tabular

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
      $(persistFileWith lowerCaseSettings "common/models")

type Scraper = SqlPersistT (NoLoggingT (ResourceT (StateT (Maybe Track) IO)))
type RPScraper = SqlPersistT (NoLoggingT (ResourceT IO))
type StationID = Int

class PersistEntity b => Saveable a b where
    toEntry :: a -> b
    fromEntry :: b -> a
--    saveEntry :: (Saveable a b c, Monad m, MonadResource m, MonadIO m) => a -> ReaderT SqlBackend m c

instance FromJSON Track where
    parseJSON (Object o) = Track <$>
                           o .: "Line1" <*>
                           o .: "Line2" <*>
                           o .: "Line3" <*>
                           o .:? "stationID" .!= 0

instance Show Track where
    show (Track ar nm al _) =
        "Artist: " ++ T.unpack ar ++ "\n" ++
        "Name: " ++ T.unpack nm ++ "\n" ++
        "Album: " ++ T.unpack al

instance Eq Track where
    (Track artist1 name1 album1 _) == (Track artist2 name2 album2 _) =
        artist1 == artist2 &&
        name1 == name2 &&
        album1 == album2

saveTrack :: (Monad m, MonadResource m, MonadIO m) => Track -> ReaderT SqlBackend m ()
saveTrack track =
    insert_ track

getStations :: (Monad m, MonadResource m, MonadIO m) => ReaderT SqlBackend m [Station]
getStations = do
  ents <- selectList [] [Asc StationName]
  return $ fmap entityVal ents

getSourcePlaylists :: (Monad m, MonadResource m, MonadIO m) => ReaderT SqlBackend m [SourcePlaylists]
getSourcePlaylists = do
  ents <- selectList [] []
  return $ fmap entityVal ents

renderStations :: [Station] -> String
renderStations stations =
    renderRows $ fmap
                   ( \(Station name id) ->
                         [T.unpack name, show id]
                   ) stations

getScrapedTracks :: (Monad m, MonadResource m, MonadIO m) => StationID -> ReaderT SqlBackend m [Track]
getScrapedTracks stationID = do
    ents <- selectList [TrackStation ==. stationID] []
    return $ fmap entityVal ents

spotifyArtistEntryKey = SpotifyArtistEntryKey

spotifyAlbumEntryKey = SpotifyAlbumEntryKey

spotifyTrackEntryKey = SpotifyTrackEntryKey

insertNotExists key val = do
  res <- Database.Persist.get key
  case res of
    Nothing -> insert_ val
    Just _ -> return ()
