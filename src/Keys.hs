{-# LANGUAGE OverloadedStrings #-}
module Keys where

import Network.OAuth.OAuth2

spotifyKey :: OAuth2
spotifyKey = OAuth2 { oauthClientId = "f643c7bbc0d14d348ef188308674b192"
                   , oauthClientSecret = "216f57bcb8fa4d14971f1ef18ab8fa1c"
                   , oauthCallback = Just "http://localhost:4242"
                   , oauthOAuthorizeEndpoint = "https://accounts.spotify.com/authorize"
                   , oauthAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
                   }

spotifyScope :: QueryParams
spotifyScope = map (\x -> ("scope", x)) scopes
    where
      scopes = [ "user-read-private"
               , "user-library-read"
               , "playlist-read-private"
               , "playlist-modify-private"
               ]
                        