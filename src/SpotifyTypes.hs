{-# LANGUAGE OverloadedStrings #-}
module SpotifyTypes where

import Data.Aeson
import Control.Monad (mzero, void)
import qualified Data.Text as T

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

data PagingObject a = PagingObject
  { href :: T.Text,
    items :: [a],
    limit :: Int,
    next :: Maybe T.Text,
    offset :: Int,
    previous :: Maybe T.Text,
    total :: Int
  }

instance (FromJSON a) => FromJSON (PagingObject a)
  where
    parseJSON (Object o) = PagingObject <$> o .: "href" <*>
                                            o .: "items" <*>
                                            o .: "limit" <*>
                                            o .:? "next" <*>
                                            o .: "offset" <*>
                                            o .:? "previous" <*>
                                            o .: "total"

    parseJSON _ = mzero
