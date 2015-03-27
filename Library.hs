module Library
  where

import Control.Monad.State

import Network.OAuth2.OAuth2
import Network.OAuth2.Util
import Resources
import Data.List.Split
import System.IO
import System.FilePath
import Util
import Data.List

getSavedTracks :: Flow ()
getSavedTracks = do
  library <- fromAuthUrl "https://api.spotify.com/v1/me/tracks" :: Flow (PagingObject SavedTrack)
  liftIO $ mapM_ (putStrLn . show) (items library)

getPlaylists :: Flow ([SimplifiedPlaylistObject])
getPlaylists = do
  currentUser <- fromAuthUrl "https://api.spotify.com/v1/me" :: Flow UserObjectPrivate
  pages <- fetchPagingObjects $ Just $ "https://api.spotify.com/v1/users/" ++ userId currentUser ++ "/playlists"
  return $ items pages

addTracks :: String -> [String] -> Flow ()
addTracks playlistId trackIds = do
  currentUser <- fromAuthUrl "https://api.spotify.com/v1/me" :: Flow UserObjectPrivate
  let urls = map (\x -> url x currentUser) $ map uriChunk chunks
  results <- mapM authRequest urls
  liftIO $ mapM_ (putStrLn . show) results
    where
      chunks = chunksOf 100 trackIds
      uriChunk ids = foldr (\x y -> x ++ "," ++ y) "" (init ids) ++ last ids
      baseUrl = "https://api.spotify.com/v1/users/"
      url chunk currentUser = baseUrl ++ userId currentUser ++ "/playlists/"
      	     	 	       ++ playlistId ++ "/tracks?uris=" ++ chunk

replacePlaylistTracks :: String -> [String] -> Flow ()
replacePlaylistTracks playlistUri trackUris = do
  currentUser <- getCurrentUser
  let playlistOwner = extractUserName playlistUri

  if userId currentUser == playlistOwner
  then do
    let urls = map (\x -> url x currentUser) $ map uriChunk chunks
    replaceResult <- authRequest' (head urls) "PUT"
    results <- mapM (\url -> authRequest url) urls
    liftIO $ mapM_ (putStrLn . show) results
  else return ()
    where
      chunks = chunksOf 100 trackUris
      uriChunk ids = foldr (\x y -> x ++ "," ++ y) "" (init ids) ++ last ids
      baseUrl = "https://api.spotify.com/v1/users/"
      playlistId = extractId playlistUri
      url chunk currentUser = baseUrl ++ userId currentUser ++ "/playlists/"
                                 ++ playlistId ++ "/tracks?uris=" ++ chunk

getTracks :: String -> Flow [PlaylistTrackObject]
getTracks href = do
  liftIO $ putStrLn "fetching objects!"
  pages <- fetchPagingObjects (Just href)
  liftIO $ putStrLn "Done fetching objects!"
  return $ items pages

writePlaylists :: [(String,SimplifiedPlaylistObject)] -> Flow ()
writePlaylists [] = return ()
writePlaylists ((path, playlist):playlists) = do
  let href = trackObjectHref $ tracks playlist
  tracks <- getTracks href
  liftIO (do
  	     putStrLn $ "Writing to " ++ path
	     withFile path WriteMode (\handle -> do
	     	      	   mapM_ ((hPutStrLn handle) . trackUri . track) tracks)
         )

  writePlaylists playlists

readPlaylists :: [(FilePath, SimplifiedPlaylistObject)] -> Flow ()
readPlaylists [] = return ()
readPlaylists ((path, playlist):playlists) = do
  trackUris <- liftIO $ getURIs path
  let (locals, remotes) = partition (isInfixOf "local") trackUris
  liftIO $ putStrLn $ simplifiedName playlist
  liftIO $ putStrLn "For now you will have to manually add these local tracks"
  liftIO $ mapM_ (putStrLn) locals

  replacePlaylistTracks (playlistUri playlist) remotes
  readPlaylists playlists
  
getSources :: FilePath -> ([(FilePath, SimplifiedPlaylistObject)] -> Flow ()) ->Flow Int
getSources path playlistAction = do
      getTokens

      uris <- liftIO $ getURIs path
      let dir = takeDirectory path

      playlists <- getPlaylists
      let desired = filter (\x -> playlistUri x `elem` uris) playlists

      -- let hrefs = map (\x -> (dir ++ "/" ++ simplifiedName x ++ ".txt", trackObjectHref $ tracks x)) desired
      -- playlistAction hrefs
      playlistAction $ map (\x -> (dir ++ "/" ++ simplifiedName x ++ ".txt", x)) desired
      
      return 0
