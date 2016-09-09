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
{-# LANGUAGE ScopedTypeVariables #-}

module Core where

import Prelude ()
import ClassyPrelude

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist
import Data.Aeson
import Data.Aeson.Types

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
    uri Text Maybe
    TrackName artist song
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
    TrackStationsUnique track station
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
    (Track a1 s1 al1 _) == (Track a2 s2 al2 _) =
        a1  == a2
     && s1  == s2
     && al1 == al2

instance FromJSON Track where
    parseJSON (Object o) = Track <$> artists
                                 <*> o .: "name"
                                 <*> album
                                 <*> o .:? "uri"
        where
          artists = artistText <$> headEx <$> (o .: "artists" :: Parser [Artist])
          album = albumText <$> o .: "album"
          artistText (Artist n) = n
          albumText (Album n) = n

-- Only inserts if the record does not exist. In the case that the
-- record exists, the key of the record is returned.
insertUnique' datum = do
  r <- checkUnique datum
  case r of
    Nothing -> insert datum
    Just uniqueVal -> do
           r' <- getBy uniqueVal
           case r' of
             Nothing -> error "Something went horribly wrong in the insertUnique' function"
             Just entity -> return $ entityKey entity

pprintTrack :: Track -> Text
pprintTrack (Track artist song album uri) =
    "Artist: " <> artist <> "\n" <>
    "Track: " <> song <> "\n" <>
    "Album: " <> album <> "\n" <>
    "Uri: "   <> tshow uri <> "\n"
