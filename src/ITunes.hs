{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module ITunes where

import ClassyPrelude
import Data.Aeson
import Internal.Types
import Servant.Client
import Servant.API
import Crypto.PubKey.ECC.P256
import Data.Proxy
import Control.Lens hiding ((.=))
import Data.Aeson.Types

type Playlists = "v1" :> "catalog" :> "us" :> "playlists" :> QueryParams "ids" PlaylistId :> Header "Authorization" JWT :> Get '[JSON] (ResponseRoot ITunesPlaylist)
-- type Playlist = "v1" :> "catalog" :> "us" :> "playlists" :> Capture "id" PlaylistId :> "tracks" :> QueryParam "offset" Offset :> Header "Authorization" JWT :> Get '[JSON] (ResponseRoot ITunesTrack)
type Playlist = "v1" :> "catalog" :> "us" :> "playlists" :> Capture "id" PlaylistId :> Header "Authorization" JWT :> Get '[JSON] (ResponseRoot ITunesTrack)

data ResponseRoot a
    = Data
      { _data :: [Resource a]
      , _offset :: (Maybe Offset)
      }
    | Results
    | Errors
    | Meta
    deriving (Show, Generic)

instance ToJSON a => ToJSON (ResponseRoot a) where
    toEncoding = genericToEncoding ( defaultOptions { fieldLabelModifier = toLower . drop 1, sumEncoding = UntaggedValue } )

instance FromJSON a => FromJSON (ResponseRoot a) where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . drop 1, sumEncoding = UntaggedValue } )

data Resource a = Resource
  { _resId :: Text
  , _resType :: Text
  , _resRelationships :: a
  } deriving (Show, Generic)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error "Really should remove this fromJust call"

instance ToJSON a => ToJSON (Resource a) where
    toEncoding = genericToEncoding ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_res", sumEncoding = UntaggedValue } )

instance FromJSON a => FromJSON (Resource a) where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_res", sumEncoding = UntaggedValue } )
  

data ITunesTrack = ITunesTrack { _unITunes :: Track }
  deriving (Show)

instance FromJSON ITunesTrack where
  parseJSON (Object o) = ITunesTrack
    <$> (Track
         <$> o .: "artistName"
         <*> o .: "name"
         <*> o .: "albumName"
         <*> o .: "isrc"
         <*> pure Nothing
        )

data ITunesPlaylist = ITunesPlaylist
     { _plTracks :: TracksData
     , _plOffset :: Maybe Offset
     } deriving (Show, Generic)

-- instance ToJSON ITunesPlaylist where
--     toEncoding = genericToEncoding ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_pl" } )

instance FromJSON ITunesPlaylist where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_pl" } )

data TracksData = TracksData
     { _trData :: [TracksDataAttr]
     } deriving (Show, Generic)

instance FromJSON TracksData where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_tr" } )

data TracksDataAttr = TracksDataAttr
     { _trdAttributes :: ITunesTrack
     } deriving (Show, Generic)

instance FromJSON TracksDataAttr where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_trd" } )

newtype Offset = Offset Int
  deriving Show

instance ToHttpApiData Offset where
  toQueryParam (Offset n) = trace "Calling it!" $ toQueryParam n

instance FromJSON Offset where
  parseJSON (Number n) = Offset <$> parseJSON (Number n)

instance ToJSON Offset where
  toJSON (Offset n) = toJSON n

newtype ITunesTrackId = ITunesTrackId Text
  deriving (Show)

instance FromJSON ITunesTrackId where
  parseJSON (Object o) = ITunesTrackId
    <$> o .: "id"
  parseJSON _ = fail "Expecting an Object when decoding ITunesTrackId"

instance ToJSON ITunesTrackId where
  toJSON (ITunesTrackId txt) = object
    ["id" .= txt
    ]

data DevToken = DevToken
  { _alg :: Text
  , _kid :: Text
  } deriving Show

newtype PlaylistId = PlaylistId Text
  deriving (Show, ToHttpApiData, Generic)

instance FromJSON PlaylistId where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = UntaggedValue} )

newtype JWT = JWT Text
  deriving Show

instance ToHttpApiData JWT where
  toHeader (JWT tok) = "Bearer " <> encodeUtf8 tok
  toUrlPiece (JWT tok) = toUrlPiece tok

data SourcePlaylist = SourcePlaylist
  { _srcName :: Text
  , _srcId :: PlaylistId
  } deriving (Show, Generic)

instance FromJSON SourcePlaylist where
  parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = toLower . fromJust . stripPrefix "_src", sumEncoding = UntaggedValue } )

makeLenses ''SourcePlaylist

playlists :: [PlaylistId] -> JWT -> ClientM (ResponseRoot ITunesPlaylist)
playlists ids jwt = client (Proxy :: Proxy Playlists) ids (Just jwt)

-- playlist :: PlaylistId -> Maybe Offset -> JWT -> ClientM (ResponseRoot ITunesTrack)
-- playlist id offset jwt = client (Proxy :: Proxy Playlist) id offset (Just jwt)

playlist :: PlaylistId -> JWT -> ClientM (ResponseRoot ITunesTrack)
playlist id jwt = client (Proxy :: Proxy Playlist) id (Just jwt)

makeLenses ''ITunesTrack
makeLenses ''Resource
makeLenses ''ITunesPlaylist
makeLenses ''TracksData
makeLenses ''TracksDataAttr
