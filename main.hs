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
import Options
import Options.Applicative

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

run :: Command -> Flow ()
run (TmpAdd path) = liftIO $ putStrLn "Adding tracks to tmp playlist"
run (ReadPlaylists path) = liftIO $ putStrLn "Updating source playlists from local files."
run (WritePlaylists path) = liftIO $ putStrLn "Updating local files from source playlists."
run (Search path) = do
  liftIO $ putStrLn "Looking for the tracks in spotify's library."
  results <- findTracks path
  liftIO $ mapM_ (\x -> do
                        case x of
                          Left msg -> putStrLn msg
                          Right track -> putStrLn $ trackUri track)
                        results

opts :: ParserInfo Command
opts = info (parseArgs <**> helper) idm

main :: IO ()
main = do
  cmd <- execParser opts
  let action = evalStateT . runExceptT $
               (\x -> do
                      getTokens
                      run x) cmd
  webFlow <- createFlow "configuration"
  result <- action webFlow
  case result of
    Left msg -> putStrLn msg
    Right act -> return act
