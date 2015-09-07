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

fetchPagingObjects :: (URI -> Flow (SpotifyPagingObject b)) -> Maybe URI -> Flow [b]
fetchPagingObjects _ Nothing = return []
fetchPagingObjects f (Just uri) = do
  logInfo "Getting page"
  page <- f uri
  let nextURI = nextPage page
  next <- fetchPagingObjects f (nextURI)
  return $ (items page) `mappend` next

getSourcePlaylists :: [SimplifiedPlaylistObject] -> [SourcePlaylists] -> [SimplifiedPlaylistObject]
getSourcePlaylists playlists desired =
    filter (\playlist -> (playlistUri playlist) `elem` desiredUUID) playlists
  where
    desiredUUID = fmap sourcePlaylistsUuid desired

getPlaylistTracks :: SimplifiedPlaylistObject -> Flow [PlaylistTrackObject]
getPlaylistTracks playlist =
    fetchPagingObjects fetchObject trackUri
  where
    trackUri = Just $ trackObjectHref trackObj
    trackObj = tracks playlist

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