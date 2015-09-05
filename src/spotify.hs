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
import Control.Monad.Logger (runNoLoggingT)

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.Logger
import System.Log.FastLogger (fromLogStr)
import Control.Monad.Trans.Resource (ResourceT)
import Network.OAuth.OAuth2 (OAuth2Result)

import SpotifyTypes
import Library
import Util

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

main :: IO ()
main = do
  confDir <- getConfDir
  createDirectoryIfMissing True confDir

  mgr <- newManager tlsManagerSettings
  curTime <- getCurrentTime

  let dbPath = (T.concat [T.pack confDir, "/db.sqlite"])
  oldFlow <- runSqlite dbPath (runMigration migrateAll >>
                               retrieveFlow "My Spotify Mixer 0.2"
                                            "MyMixer"
                              )

  let runDB = (runSqlite dbPath)
      fn = (evalStateT . runExceptT . (flip runLoggingT myLogger) . runDB)
         ( do
             runMigration migrateAll
             playlists <- getUserPlaylists
             desired <- selectList [] []
             let desiredPlaylists = getSourcePlaylists playlists $ fmap entityVal desired
             trackObjs <- sequence $ fmap getPlaylistTracks desiredPlaylists
             return $ fmap (fmap track) trackObjs
         )
         (createFlow oldFlow curTime mgr)
  res <- fn

  mapM_ (mapM_ (putStrLn . show)) res