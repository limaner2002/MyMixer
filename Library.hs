module Library
  where

import Control.Monad.State

import Network.OAuth2.OAuth2
import Network.OAuth2.Util
import Resources
import Data.List.Split
import System.IO

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

getTracks :: String -> Flow [PlaylistTrackObject]
getTracks href = do
  liftIO $ putStrLn "fetching objects!"
  pages <- fetchPagingObjects (Just href)
  liftIO $ putStrLn "Done fetching objects!"
  return $ items pages

getAllTracks :: [(String, String)] -> Flow ()
getAllTracks [] = return ()
getAllTracks ((path, href):playlists) = do
  tracks <- getTracks href

--   liftIO $ putStrLn "-----------Playlist start-----------"
--   liftIO $ putStrLn $ "Playlist name: " ++ name
--   liftIO $ mapM_ (putStrLn . show . track) tracks -- putStrLn $ show $ track tracks
--   liftIO $ putStrLn "------------Playlist end------------"
  liftIO (do
  	     putStrLn $ "Writing to " ++ path
	     withFile path WriteMode (\handle -> do
	     	      	   mapM_ ((hPutStrLn handle) . trackUri . track) tracks)
         )

  getAllTracks playlists