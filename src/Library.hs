{-# LANGUAGE OverloadedStrings #-}
module Library where

import Flow
import SpotifyTypes
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON)

getCurrentUser :: Flow UserObjectPrivate
getCurrentUser = do
    eUser <- flowGetJSON "https://api.spotify.com/v1/me"
    checkResult eUser

getUserPlaylists :: Flow [SimplifiedPlaylistObject]
getUserPlaylists = do
  currentUser <- getCurrentUser
  let uri = T.concat [base, uid, "/playlists"]
      base = "https://api.spotify.com/v1/users/"
      uid = userId currentUser
  fetchPagingObjects fetchObject (Just uri)

checkResult :: Either BL.ByteString a -> Flow a
checkResult (Left msg) = throwError msg
checkResult (Right val) = return val

fetchPagingObjects :: (URI -> Flow (PagingObject b)) -> Maybe URI -> Flow [b]
fetchPagingObjects _ Nothing = return []
fetchPagingObjects f (Just uri) = do
  page <- f uri
  let nextURI = next page
  next <- fetchPagingObjects f (nextURI)
  return $ (items page) `mappend` next

fetchObject :: FromJSON a => (URI -> Flow a)
fetchObject = (\x -> flowGetJSON x >>= checkResult)
