{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SpotifyTypes where

import Data.Aeson
import Control.Monad (mzero, void)
import qualified Data.Text as T

import Flow (fetchObject, logInfo)
import Types

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

data PlaylistTrackObject = PlaylistTrackObject
  { addedAt :: T.Text,
    track :: SpotifyTrack
  }

instance FromJSON PlaylistTrackObject
  where
    parseJSON (Object o) = PlaylistTrackObject <$>
                             o .: "added_at" <*>
                             o .: "track"
    parseJSON _ = mzero

data SpotifyTrack = SpotifyTrack
  {
     spotifyTrackName :: T.Text,
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
     show (SpotifyTrack trackName artists _ _) = (T.unpack $ artistName $ head artists) ++ "\t" ++ T.unpack trackName
     show (LocalTrack _ _ title artist album) = T.unpack title ++ "\t" ++ T.unpack artist ++ "\t" ++ T.unpack album

instance Saveable SpotifyTrack SpotifyTrackEntry where
    toEntry (SpotifyTrack {..}) =
        SpotifyTrackEntry
          { spotifyTrackEntryName = spotifyTrackName
          , spotifyTrackEntryArtists = Nothing
          , spotifyTrackEntryAlbum = Nothing
          , spotifyTrackEntryUri = trackUri
          }

-- toTrackEntry :: SpotifyTrack -> Maybe SpotifyArtistEntryId -> Maybe SpotifyAlbumEntryId -> SpotifyTrackEntry
-- toTrackEntry track artistId albumId =
--     SpotifyTrackEntry u n artistId albumId
--   where
--     u = trackUri track
--     n = spotifyTrackName track

saveSpotifyTrack :: (Monad m, MonadResource m, MonadIO m) => SpotifyTrack -> ReaderT SqlBackend m ()
saveSpotifyTrack track = do
  let mainArtist = head $ artists track
      mainArtistEntry = toEntry mainArtist :: SpotifyArtistEntry
      albumEntry = toEntry $ album track :: SpotifyAlbumEntry
      trackEntry = toEntry track :: SpotifyTrackEntry

      artistId = spotifyArtistEntryKey $ artistUri mainArtist
      albumId = spotifyAlbumEntryKey $ albumUri $ album track
      trackId = spotifyTrackEntryKey $ trackUri track

  insertNotExists artistId mainArtistEntry
  insertNotExists albumId albumEntry
  insertNotExists trackId trackEntry
              { spotifyTrackEntryArtists = (Just artistId)
              , spotifyTrackEntryAlbum = (Just albumId)
              }

  -- insert_ mainArtist
  -- insert_ albumEntry

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
  , artistUri :: T.Text
  } deriving Show

instance FromJSON Artist
  where
     parseJSON (Object o) = Artist <$>
                            o .: "name" <*>
                            o .: "uri"

     parseJSON _ = mzero

instance Saveable Artist SpotifyArtistEntry where
    toEntry (Artist {..}) =
        SpotifyArtistEntry
        { spotifyArtistEntryName = artistName
        , spotifyArtistEntryUri = artistUri
        }

data Album = Album
  {
    albumName :: T.Text
  , albumUri :: T.Text
  } deriving Show

instance FromJSON Album
  where
     parseJSON (Object o) = Album <$>
                            o .: "name" <*>
                            o .: "uri"                            

     parseJSON _ = mzero

instance Saveable Album SpotifyAlbumEntry where
    toEntry (Album {..}) =
        SpotifyAlbumEntry
        { spotifyAlbumEntryName = albumName
        , spotifyAlbumEntryUri = albumUri
        }

data TrackList = TrackList (SpotifyPagingObject SpotifyTrack)

instance FromJSON TrackList
    where
      parseJSON (Object o) = TrackList <$>
                             o .: "tracks"
      parseJSON _ = mzero