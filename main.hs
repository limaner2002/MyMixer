import Resources
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
import Util

tmpAdd :: FilePath -> Flow ()
tmpAdd path = do
  ids <- liftIO $ getURIs path
  replacePlaylistTracks "spotify:user:limaner2002:playlist:0Af5MOLY5avsx3nbF7grNs" ids

  -- let chunks = chunksOf 50 $ map extractId ids
  -- let urls = map (\idChunk -> "https://api.spotify.com/v1/tracks/?ids=" ++ foldr (\x y -> x ++ "," ++ y) "" (init idChunk) ++ last idChunk) chunks

  -- tracks <- fetchSeveral urls :: Flow Tracks

  -- liftIO $ putStrLn $ show tracks

start :: FilePath -> Flow Int
start path = do
      getTokens
      -- tmpAdd path
      getSources path readPlaylists
      -- getSources path writePlaylists
      
      -- results <- findTracks path
      -- liftIO $ mapM_ (\x -> do
      --                       case x of
      --                         Left msg -> putStrLn msg
      --                         Right track -> putStrLn $ trackUri track) results

      return 0

main = do
     (path:args) <- getArgs
     webFlow <- createFlow "configuration"
     let t = evalStateT . runExceptT $ start path 
     retVal <- t webFlow
     putStrLn $ show retVal
