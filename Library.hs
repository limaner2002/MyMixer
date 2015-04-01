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
import Network.HTTP (urlEncode)

import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types

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
  liftIO $ mapM_ (putStrLn . show . snd) results
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
    -- replaceResult <- authRequest' (head urls) "PUT"
    -- results <- mapM (\url -> authRequest url) urls
    results <- replace urls
    liftIO $ mapM_ (putStrLn . show . snd) results
  else return ()
    where
      chunks = chunksOf 100 trackUris
      uriChunk ids = foldr (\x y -> x ++ "," ++ y) "" (init ids) ++ last ids
      baseUrl = "https://api.spotify.com/v1/users/"
      playlistId = extractId playlistUri
      url chunk currentUser = baseUrl ++ userId currentUser ++ "/playlists/"
                                 ++ playlistId ++ "/tracks?uris=" ++ chunk
      replace [] = return []
      replace (url:urls) = do
        replaceResult <- authRequest' url "PUT"
        results <- mapM (\url -> authRequest url) urls
        return $ replaceResult : results

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
      -- getTokens

      uris <- liftIO $ getURIs path
      let dir = takeDirectory path

      playlists <- getPlaylists
      let desired = filter (\x -> playlistUri x `elem` uris) playlists

      -- let hrefs = map (\x -> (dir ++ "/" ++ simplifiedName x ++ ".txt", trackObjectHref $ tracks x)) desired
      -- playlistAction hrefs
      playlistAction $ map (\x -> (dir ++ "/" ++ simplifiedName x ++ ".txt", x)) desired
      
      return 0

spotifySearch :: SpotifyQuery -> Flow (Either String Track)
spotifySearch query = do
  (body, _) <- authRequest' url "GET"
  let obj = decode (body) :: Maybe (M.Map String Value)
  case obj of
    Nothing -> do
             liftIO $ print "Failed to decode!"
             return msg
    Just thing -> do
             let item = M.lookup "tracks" thing
             case item of
               Nothing -> do
                         liftIO $ print "Does not exist!"
                         return msg
               Just page -> do
                         let result = fromJSON page :: Result (PagingObject Track)
                         case result of
                           Error s -> do
                                       liftIO $ print s
                                       return msg
                           Success pageObject -> do
                                       if length (items pageObject) > 0
                                       then return $ Right $ head $ items pageObject
                                       else return msg
 where
   baseUrl = "https://api.spotify.com/v1/search?"
   url = baseUrl ++ "q=artist:" ++ urlEncode (queryArtist query) ++ "+track:" ++ urlEncode (queryTrack query) ++ "&type=track"
   msg = Left $ "Could not find " ++ queryArtist query ++ "\t" ++ queryTrack query

findTracks :: FilePath -> Flow ([Either String Track])
findTracks path = do
  queryLines <- liftIO $ withFile path ReadMode (\handle -> getLines handle)
  let queries = map (\x -> query x) queryLines
  mapM spotifySearch queries
 where
   query x = SpotifyQuery (artist x) (track x)
   artist x = (l x) !! 0
   track x = (l x) !! 1
   l x = splitOn "\t" x
