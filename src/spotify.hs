{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import System.Environment
import           Network.HTTP.Conduit
import Flow
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Keys
import Data.Time
import System.Directory
import Database.Persist (insert)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Model
import qualified Data.Text as T
import Control.Monad.Logger (runNoLoggingT)

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)


getConfDir :: IO String
getConfDir = do
#ifdef darwin_HOST_OS
       home <- getHomeDirectory
       return $ home ++ "/Library/Application Support/Me" ++ "/MyMixer"
#else
       getAppUserDataDirectory "MyMixer"
#endif

main :: IO ()
main = do
  confDir <- getConfDir
  createDirectoryIfMissing True confDir
  let runDB = (runSqlite (T.concat [T.pack confDir, "/db.sqlite"]))

  mgr <- newManager tlsManagerSettings
  curTime <- getCurrentTime
  res <- (evalStateT . runExceptT . runDB)
         (runMigration migrateAll >> getToken) (
                   OAuth2WebServerFlow
                   { flowToken = Nothing
                   , oauth2 = spotifyKey
                   , manager = mgr
                   , flowScope = spotifyScope
                   , timestamp = curTime
                   , authService = "My Spotify Mixer 0.2"
                   , authAccount = "MyMixer"
                   }
                  )
  print res

