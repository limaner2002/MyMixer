{-# LANGUAGE OverloadedStrings #-}
module SpotifyTypes where

import Data.Aeson
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

-- data SimplifiedPlaylistObject = SimplifiedPlaylistObject
--   { simplifiedName :: String,
--     playlistUri :: String,
--     playlistHref :: String,
--     tracks :: TrackObject
--   }

-- instance FromJSON SimplifiedPlaylistObject
--   where
--     parseJSON (Object o) = SimplifiedPlaylistObject <$>
--                                o .: "name" <*>
--                                o .: "uri" <*>
--                                o .: "href" <*>
--                                o .: "tracks"
--     parseJSON _ = mzero
