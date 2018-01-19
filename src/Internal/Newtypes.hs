{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Newtypes where

import ClassyPrelude
import ProjectM36.Client.Simple
import ProjectM36.Tupleable
import Data.Binary

newtype ArtistName = ArtistName Text
  deriving (Show, Eq, Generic, NFData, Binary, IsString)

newtype SongName = SongName Text
  deriving (Show, Eq, Generic, NFData, Binary, IsString)

newtype AlbumName = AlbumName Text
  deriving (Show, Eq, Generic, NFData, Binary, IsString)

newtype TrackId = TrackId Int
  deriving (Show, Eq, Generic, NFData, Binary, Num)

newtype StationId = StationId Int
  deriving (Show, Eq, Generic, NFData, Binary, Num)

newtype StationName = StationName Text
  deriving (Show, Eq, Generic, NFData, Binary, IsString)

newtype SeenCount = SeenCount Int
  deriving (Show, Eq, Generic, NFData, Binary, Num)

newtype Isrc = Isrc Text
  deriving (Show, Eq, Generic, NFData, Binary, IsString)

