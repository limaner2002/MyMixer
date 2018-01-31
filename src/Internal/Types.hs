{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Types 
       ( module Internal.Types
       , module Internal.Newtypes
       ) where

import ClassyPrelude
import ProjectM36.Client.Simple
import ProjectM36.Tupleable
import Data.Binary
import Internal.Newtypes
import Data.Aeson
import Control.Lens

data Track = Track
  { _artist :: ArtistName
  , _song :: SongName
  , _album :: Maybe AlbumName
  , _isrc :: Isrc
  , _trackId :: Maybe TrackId
  } deriving (Show, Eq, Generic, Atomable, NFData, Binary)

instance Tupleable Track

data Station = Station
  { _stationId :: StationId
  , _stationName :: StationName
  } deriving (Show, Eq, Generic, Atomable, NFData, Binary)

instance Tupleable Station

data TrackStations = TrackStations
  { _track :: TrackId
  , _station :: StationId
  , _seen :: SeenCount
  } deriving (Show, Eq, Generic, Atomable, NFData, Binary)

instance Tupleable TrackStations

instance Atomable ArtistName
instance Atomable SongName
instance Atomable AlbumName
instance Atomable TrackId
instance Atomable StationId
instance Atomable StationName
instance Atomable SeenCount
instance Atomable Isrc

instance ToJSON ArtistName
instance FromJSON ArtistName

instance ToJSON SongName
instance FromJSON SongName

instance ToJSON AlbumName
instance FromJSON AlbumName

instance ToJSON TrackId
instance FromJSON TrackId

instance ToJSON Isrc
instance FromJSON Isrc

makeLenses ''Track
makeLenses ''Station
makeLenses ''TrackStations
