{-# LANGUAGE OverloadedStrings #-}
module Resources where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Monoid
import Network.HTTP.Conduit
import Network.OAuth2.OAuth2
import Network.OAuth2.Util
import Network.HTTP.Types.Status (Status(..))
import Data.Generics

data Artist = Artist
  {
     artistName :: String
  } deriving Show

instance FromJSON Artist
  where
     parseJSON (Object o) = Artist <$> o .: "name"

     parseJSON _ = mzero

data Album = Album
  {
     albumName :: String
  } deriving Show

instance FromJSON Album
  where
     parseJSON (Object o) = Album <$> o .: "name"

     parseJSON _ = mzero

data Track = Track
  {
     trackName :: String,
     artists :: [Artist],
     album :: Album,
     trackUri :: String
  } 

instance Show Track
  where
     show (Track trackName artists _ _) = trackName ++ "\t" ++ (artistName $ head artists)

instance FromJSON Track
  where
     parseJSON (Object o) = Track <$> o .: "name" <*>
     	       	       	    	      o .: "artists" <*>
				      o .: "album" <*>
				      o .: "uri"

     parseJSON _ = mzero

data Tracks = Tracks
  {
     trackList :: [Track]
  }

instance FromJSON Tracks
  where
     parseJSON (Object o) = Tracks <$> o .: "tracks"

     parseJSON _ = mzero

instance Show Tracks
  where
     show tracks = do
     	  let tList = map (\track -> artist track ++ "\t" ++ trackName track) $ trackList tracks
     	  foldr (\x y -> x ++ "\n" ++ y) "" $ tList
     	     where
		artist track = artistName . head $ artists track

instance Monoid Tracks
  where
     mempty = Tracks []
     mappend (Tracks l) (Tracks r) = Tracks (l ++ r)

data PagingObject a = PagingObject
  { href :: String,
    items :: [a],
    limit :: Int,
    next :: Maybe String,
    offset :: Int,
    previous :: Maybe String,
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

instance Monoid (PagingObject a)
  where
    mempty = PagingObject [] [] 0 Nothing 0 Nothing 0
    mappend (PagingObject _ lItems _ _ _ _ lTotal)
    	    (PagingObject _ rItems _ _ _ _ rTotal) = 
	    		  PagingObject [] (lItems ++ rItems) 0 Nothing 0 Nothing (lTotal + rTotal)

data SavedTrack = SavedTrack
  { added_at :: String,
    savedTrack :: Track
  }

instance FromJSON SavedTrack
  where
    parseJSON (Object o) = SavedTrack <$> o .: "added_at" <*>
    	      	      	   	      	  o .: "track"
    parseJSON _ = mzero

instance Show SavedTrack
  where
    show = show . savedTrack

data UserObjectPrivate = UserObjectPrivate
  { birthdate :: Maybe String,
    country :: String,
    display_name :: Maybe String,
    email :: Maybe String,
    userHref :: String,
    userId :: String,
    product :: String,
    userType :: String,
    uri :: String
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

    parseJSON _ = mzero

data SimplifiedPlaylistObject = SimplifiedPlaylistObject
  { simplifiedName :: String,
    playlistUri :: String,
    playlistHref :: String,
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
--     parseJSON value = do
--       withObject "SimplifiedPlaylistObject" (\obj -> do
--       		     name <- obj .: "name"
-- 		     uri <- obj .: "uri"
-- 		     href <- obj .: "href"
-- 		     tracks <- (parseJSON value :: Parser PlaylistTrackObject)
-- 		     return $ SimplifiedPlaylistObject 
-- 		     	        name uri href (Left tracks)
-- 		     ) value
--        parseJSON value = do
--          withObject "SimplifiedPlaylistObject" (\obj -> do
-- 	 	      SimplifiedPlaylistObject <$>
-- 			obj .: "name" <*>
-- 			obj .: "uri" <*>
-- 			obj .: (parseJSON value :: Parser SimplifiedPlaylistObject)
-- 			    .!= (parseJSON value :: Parser [Track])
-- 		     ) value

instance Show SimplifiedPlaylistObject
  where
    show (SimplifiedPlaylistObject name uri _ _) = name ++ "\t" ++ uri

data FullPlaylistObject = FullPlaylistObject
  { fullName :: String,
    fullUri :: String,
    fullHref :: String,
--     playlistBase :: SimplifiedPlaylistObject,
    snapshotId :: String,
    fullTracks :: [Track]
  }

instance FromJSON FullPlaylistObject
  where
    parseJSON (Object o) = FullPlaylistObject <$>
    	      	      	     o .: "name" <*>
			     o .: "uri" <*>
			     o .: "href" <*>
			     o .: "snapshot_id" <*>
			     o .: "tracks"
    parseJSON _ = mzero

data TrackObject = TrackObject
  { trackObjectHref :: String,
    trackObjectTotal :: Int
  }

instance FromJSON TrackObject
  where
    parseJSON (Object o) = TrackObject <$>
    	      	      	     o .: "href" <*>
			     o .: "total"

    parseJSON _ = mzero

data PlaylistTrackObject = PlaylistTrackObject
  { addedAt :: String,
--     addedBy :: String,
    track :: Track
  }

instance FromJSON PlaylistTrackObject
  where
    parseJSON (Object o) = PlaylistTrackObject <$>
    	      	      	     o .: "added_at" <*>
-- 			     o .: "added_by" <*>
			     o .: "track"
    parseJSON _ = mzero


data SpotifyQuery = SpotifyQuery
    { queryArtist :: String,
      queryTrack :: String
    }
