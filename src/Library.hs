{-# LANGUAGE OverloadedStrings #-}
module Library where

import Flow
import SpotifyTypes

getCurrentUser :: Flow UserObjectPrivate
getCurrentUser = do
    eUser <- flowGetJSON "https://api.spotify.com/v1/me"
    case eUser of
      Left msg -> throwError msg
      Right user -> return user