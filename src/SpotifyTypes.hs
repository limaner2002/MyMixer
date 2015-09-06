{-# LANGUAGE OverloadedStrings #-}
module SpotifyTypes where

import Data.Aeson
import Control.Monad (mzero, void)
import qualified Data.Text as T
import Flow (fetchObject)

data UserObjectPrivate = UserObjectPrivate
  { birthdate :: Maybe T.Text,
    country :: T.Text,
    display_name :: Maybe T.Text,
    email :: Maybe T.Text,
    userHref :: T.Text,
    userId :: T.Text,
    product :: T.Text,
    userType :: T.Text,
    uri :: T.Text
  } deriving Show

instance FromJSON UserObjectPrivate
  where
    parseJSON (Object o) = UserObjectPrivate <$>
                               o .:? "birthdate" <*>
                               o .: "country" <*>
                               o .:? "display_name" <*>
                               o .:? "email" <*>
                               o .: "href" <*>
                               o .: "id" <*>
                               o .: "product" <*>
                               o .: "type" <*>
                               o .: "uri"

data SimplifiedPlaylistObject = SimplifiedPlaylistObject
  { simplifiedName :: T.Text,
    playlistUri :: T.Text,
    playlistHref :: T.Text,
    tracks :: TrackObject
  }

instance FromJSON SimplifiedPlaylistObject
  where
    parseJSON (Object o) = SimplifiedPlaylistObject <$>
                               o .: "name" <*>
                               o .: "uri" <*>
                               o .: "href" <*>
                               o .: "tracks"
    parseJSON _ = mzero

instance Show SimplifiedPlaylistObject
  where
    show (SimplifiedPlaylistObject name uri _ _) = T.unpack $ T.concat [name, "\t", uri]

data TrackObject = TrackObject
  { trackObjectHref :: T.Text,
    trackObjectTotal :: Int
  }

instance FromJSON TrackObject
  where
    parseJSON (Object o) = TrackObject <$>
                             o .: "href" <*>
                             o .: "total"

    parseJSON _ = mzero

data SpotifyPagingObject a = SpotifyPagingObject
  { href :: T.Text,
    items :: [a],
    limit :: Int,
    nextPage :: Maybe T.Text,
    offset :: Int,
    previous :: Maybe T.Text,
    total :: Int
  }

instance (FromJSON a) => FromJSON (SpotifyPagingObject a)
  where
    parseJSON (Object o) = SpotifyPagingObject <$> o .: "href" <*>
                                            o .: "items" <*>
                                            o .: "limit" <*>
                                            o .:? "next" <*>
                                            o .: "offset" <*>
                                            o .:? "previous" <*>
                                            o .: "total"

    parseJSON _ = mzero

-- data FullPlaylistObject = FullPlaylistObject
--   { fullName :: T.Text,
--     fullUri :: T.Text,
--     fullHref :: T.Text,
--     snapshotId :: T.Text,
--     fullTracks :: [Track]
--   }

-- instance FromJSON FullPlaylistObject
--   where
--     parseJSON (Object o) = FullPlaylistObject <$>
--                              o .: "name" <*>
--                              o .: "uri" <*>
--                              o .: "href" <*>
--                              o .: "snapshot_id" <*>
--                              o .: "tracks"
--     parseJSON _ = mzero

data PlaylistTrackObject = PlaylistTrackObject
  { addedAt :: T.Text,
--     addedBy :: String,
    track :: SpotifyTrack
  }

instance FromJSON PlaylistTrackObject
  where
    parseJSON (Object o) = PlaylistTrackObject <$>
                             o .: "added_at" <*>
--                           o .: "added_by" <*>
                             o .: "track"
    parseJSON _ = mzero

data SpotifyTrack = SpotifyTrack
  {
     trackName :: T.Text,
     artists :: [Artist],
     album :: Album,
     trackUri :: T.Text
  } | LocalTrack
  {  localPath :: T.Text,
     localUri :: T.Text,
     localTitle :: T.Text,
     localArtist :: T.Text,
     localAlbum :: T.Text
  }

instance Show SpotifyTrack
  where
     show (SpotifyTrack trackName artists _ _) = show trackName ++ "\t" ++ (show $ artistName $ head artists)
     show (LocalTrack _ _ title artist album) = show title ++ "\t" ++ show artist ++ "\t" ++ show album

instance FromJSON SpotifyTrack
  where
     parseJSON (Object o) = SpotifyTrack <$> o .: "name" <*>
                                      o .: "artists" <*>
                                      o .: "album" <*>
                                      o .: "uri"

     parseJSON _ = mzero

data Artist = Artist
  {
     artistName :: T.Text
  } deriving Show

instance FromJSON Artist
  where
     parseJSON (Object o) = Artist <$> o .: "name"

     parseJSON _ = mzero

data Album = Album
  {
     albumName :: T.Text
  } deriving Show

instance FromJSON Album
  where
     parseJSON (Object o) = Album <$> o .: "name"

     parseJSON _ = mzero
