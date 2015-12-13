{-# LANGUAGE OverloadedStrings #-}
module Library where

import Flow
import Types (SourcePlaylists, sourcePlaylistsUuid, Track (..))
import SpotifyTypes
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (urlEncode)
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, encode)

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

-- getFullPlaylist :: SimplifiedPlaylistObject -> PlaylistObject

getPlaylist :: SpotifyUri -> Flow SimplifiedPlaylistObject
getPlaylist (SpotifyUri _ _ uid _ playlistId) = do
  currentUser <- getCurrentUser
  let uri = T.concat [base, uid, "/playlists/", playlistId]
      base = "https://api.spotify.com/v1/users/"
  fetchObject uri

fetchPagingObjects :: (URI -> Flow (SpotifyPagingObject b)) -> Maybe URI -> Flow [b]
fetchPagingObjects _ Nothing = return []
fetchPagingObjects f (Just uri) = do
  logInfo "Getting page"
  page <- f uri
  let nextURI = nextPage page
  next <- fetchPagingObjects f (nextURI)
  return $ (items page) `mappend` next

pagingSource :: FromJSON a => URI -> Source Flow a
pagingSource uri = do
  page <- lift $ flowGetJSONe uri
  case nextPage page of
    Nothing -> CC.yieldMany $ items page
    Just nextURI -> do
      CC.yieldMany $ items page
      pagingSource nextURI

getPlaylistTracks :: Conduit PlaylistTrackObject Flow SpotifyTrack
getPlaylistTracks = CC.mapM (return . track)

printConsumer :: Show a => Consumer a Flow ()
printConsumer = CC.mapM_ (liftIO . print)

findTrack :: Track -> Flow (Maybe SpotifyTrack)
findTrack track = do
  res <- flowGetJSON searchURI :: Flow (Either BL.ByteString TrackList)
  case res of
    Left msg -> throwError msg
    Right (TrackList trackList) ->
        case items trackList of
          [] -> do
             logInfo notFoundMsg
             return Nothing
          tracks -> return $ Just $ head tracks
 where
   baseUrl = "https://api.spotify.com/v1/search?"
   searchURI = T.concat [baseUrl, "q=artist:", textUrlEncode (trackArtist track), "+track:", textUrlEncode (trackName track), "&type=track"]
   notFoundMsg = T.concat ["Could not find track ", trackArtist track, " - ", trackName track, "\nQuery URL: ", searchURI]
   textUrlEncode = T.pack . C8.unpack . (urlEncode False) . C8.pack . T.unpack

addTracks :: [SpotifyTrack] -> SimplifiedPlaylistObject -> Flow ()
addTracks tracks playlist = do
    let uri = T.concat [base, uid, "/playlists/", playlistId]

    res <- flowPostBS uri body
    liftIO $ print res
  where
    (SpotifyUri _ _ uid _ playlistId) = newSpotifyUri $ playlistUri playlist
    base = "https://api.spotify.com/v1/users/"
    body = [("Body", BL.toStrict $ encode uriList)]
    uriList = UriList trackUris
    trackUris = fmap trackUri tracks