{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spotify where

import ClassyPrelude
import Servant.Client
import Servant.API
import Internal.Types
import Data.Proxy
import Data.Aeson

type TrackByIsrc = "v1" :> "search" :> QueryParam "q" ByIsrc :> QueryParam "type" TrackType :> Header "Authorization" AuthToken :> Get '[JSON] Value
type TokenRequest = "api" :> "token" :> Get '[JSON] Value

newtype ByIsrc = ByIsrc Isrc
    deriving Show

instance ToHttpApiData ByIsrc where
    toQueryParam (ByIsrc (Isrc isrc)) = "isrc:" <> isrc

data TrackType = TrackType

instance ToHttpApiData TrackType where
    toQueryParam _ = "track"

newtype AuthToken = AuthToken Text
    deriving (Show, Eq, IsString)

instance ToHttpApiData AuthToken where
    toHeader (AuthToken tok) = "Bearer " <> encodeUtf8 tok
    toUrlPiece (AuthToken tok) = toUrlPiece tok

trackByIsrc :: Isrc -> AuthToken -> ClientM Value
trackByIsrc isrc tok = client (Proxy :: Proxy TrackByIsrc) (Just $ ByIsrc isrc) (Just TrackType) (Just tok)