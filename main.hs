import Resources
import System.IO
import Data.List.Split
import System.Environment
import Network.OAuth2.OAuth2
import Network.OAuth2.Util
import Network.HTTP.Conduit
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Library
import System.FilePath

getLines :: Handle -> IO [String]
getLines handle = do
       isEOF <- hIsEOF handle
       if isEOF
       then return []
       else do
	    line <- hGetLine handle
	    next <- getLines handle
	    return $ line : filter (\x -> x /= []) next

extractId :: String -> String
extractId uri = last $ splitOn ":" uri

getIDs :: FilePath -> IO [String]
getIDs path = do
       withFile path ReadMode (\handle -> do
       		     getLines handle)

printTracks :: Maybe Tracks -> IO ()
printTracks Nothing = return ()
printTracks (Just trackList) = do
	    putStrLn $ show trackList

start :: FilePath -> Flow Int
start path = do
      getTokens
      ids <- liftIO $ getIDs path

--       getSavedTracks

--       playlists <- getPlaylists
--       liftIO $ mapM_ (putStrLn . show) playlists

      addTracks "0Af5MOLY5avsx3nbF7grNs" ids

--       let chunks = chunksOf 50 $ map extractId ids
--       let urls = map (\idChunk -> "https://api.spotify.com/v1/tracks/?ids=" ++ foldr (\x y -> x ++ "," ++ y) "" (init idChunk) ++ last idChunk) chunks

--       tracks <- fetchSeveral urls :: Flow Tracks

--       liftIO $ putStrLn $ show tracks

      return 0

-- start :: FilePath -> Flow Int
-- start path = do
--       getTokens

--       ids <- liftIO $ getIDs path
--       let dir = takeDirectory path

--       playlists <- getPlaylists
--       let desired = filter (\x -> playlistUri x `elem` ids) playlists

--       let hrefs = map (\x -> (dir ++ "/" ++ simplifiedName x ++ ".txt", trackObjectHref $ tracks x)) desired
--       getAllTracks hrefs
      
--       return 0

main = do
--      (path:args) <- getArgs
--      webFlow <- createFlow "configuration"


     (path:args) <- getArgs
     webFlow <- createFlow "configuration"
     let t = evalStateT . runExceptT $ start path 
     retVal <- t webFlow
     putStrLn $ show retVal
--      (path:args) <- getArgs
--      ids <- getIDs path
--      let chunks = chunksOf 50 $ map extractId ids
--      let urls = map (\idChunk -> "https://api.spotify.com/v1/tracks/?ids=" ++ foldr (\x y -> x ++ "," ++ y) "" (init idChunk) ++ last idChunk) chunks
--      withManager (\manager -> do
--      		 	      -- (tracks, status) <- liftIO $ fromUrl manager (head urls) []
-- 			      tracks <- liftIO $ fetchTracks manager urls
-- 			      liftIO $ putStrLn $ show tracks
--      		 )
