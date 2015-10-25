{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import System.Environment
import           Network.HTTP.Conduit
import Flow
import Control.Monad.Trans.State
import Control.Monad.State (lift)
import Control.Monad.Trans.Except
import Keys
import Data.Time
import System.Directory
import Database.Persist (insert, selectList, entityVal)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Model
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.Logger (runNoLoggingT, runLoggingT)

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlPersistT)
import System.Log.FastLogger (fromLogStr)
import Control.Monad.Trans.Resource (ResourceT)
import Network.OAuth.OAuth2 (OAuth2Result)
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)

import Options.Applicative

import Options
import SpotifyTypes
import Library
import Util
import Types

-- instance Show BL.ByteString where
--     show = BL.unpack

-- instance Show C8.ByteString where
--     show = BL.unpack

getConfDir :: IO String
getConfDir = do
#ifdef darwin_HOST_OS
       home <- getHomeDirectory
       return $ home ++ "/Library/Application Support/Me" ++ "/MyMixer"
#else
       getAppUserDataDirectory "MyMixer"
#endif

createFlow :: Maybe OAuth2WebServerFlow -> UTCTime -> Manager -> OAuth2WebServerFlow
createFlow (Just oldFlow) _ mgr = oldFlow
                           { oauth2 = spotifyKey
                           , manager = mgr
                           , flowScope = spotifyScope
                           , authService = "My Spotify Mixer 0.2"
                           , authAccount = "MyMixer"
                           }

createFlow Nothing curTime mgr = 
    OAuth2WebServerFlow
    { flowToken = Nothing
    , oauth2 = spotifyKey
    , manager = mgr
    , flowScope = spotifyScope
    , timestamp = curTime
    , authService = "My Spotify Mixer 0.2"
    , authAccount = "MyMixer"
    }

findTracks :: Int -> Flow ()
findTracks stationId = do
  scraped <- getScrapedTracks stationId
  mTracks <- sequence $ fmap findTrack scraped
  let tracks = catMaybes mTracks
  liftIO $ mapM_ (putStrLn . T.unpack . trackUri) tracks
  liftIO $ mapM_ print tracks

getDesiredPlaylists :: Flow [SimplifiedPlaylistObject]
getDesiredPlaylists = do
  playlists <- getUserPlaylists
  desired <- selectList [] []
  return $ getSourcePlaylists playlists $ fmap entityVal desired

saveMix :: Flow ()
saveMix = do
  desiredPlaylists <- getDesiredPlaylists
  trackObjs <- sequence $ fmap getPlaylistTracks desiredPlaylists
  let tracks = fmap (fmap track) trackObjs
  logInfo "Saving tracks"
  mapM_ (mapM_ saveSpotifyTrack) tracks

dispatch :: Command -> Flow ()
dispatch (FindTracks stationId) = findTracks stationId
dispatch DisplayStations = showStations

showStations :: Flow ()
showStations = do
  stations <- getStations
  liftIO $ putStrLn $ renderStations stations

opts :: ParserInfo Command
opts = info (parseArgs <**> helper) idm

main :: IO ()
main = do
  cmd <- execParser opts
  dbPath <- getDBPath "db.sqlite"

  mgr <- newManager tlsManagerSettings
  curTime <- getCurrentTime

  oldFlow <- runSqlite dbPath (runMigration migrateAll >>
                               retrieveFlow "My Spotify Mixer 0.2"
                                            "MyMixer"
                              )

  let runDB = (runSqlite dbPath)
      fn = (evalStateT . runExceptT . (flip runLoggingT myLogger) . runDB)
         ( do
             runMigration migrateAll
             runMigration migrateFlow
             dispatch cmd
         )
         (createFlow oldFlow curTime mgr)
  res <- fn
  case res of
    Left msg -> putStrLn $ BL.unpack msg
    Right val -> return val