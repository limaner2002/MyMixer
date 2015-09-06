{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Util where

import System.Directory ( createDirectoryIfMissing
                        , getHomeDirectory
                        , getAppUserDataDirectory
                        )
import qualified Data.Text as T
import Control.Monad.Logger
import System.Log.FastLogger (fromLogStr)
import Data.Time
import qualified Data.ByteString.Char8 as C8

myLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
myLogger _ _ LevelInfo msg = do
  curTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let localTime = utcToLocalTime timeZone curTime
  putStrLn $ show localTime ++ ": " ++ (C8.unpack $ fromLogStr msg)
myLogger _ _ _ _ = return ()

getConfDir :: IO String
getConfDir = do
#ifdef darwin_HOST_OS
       home <- getHomeDirectory
       return $ home ++ "/Library/Application Support/Me" ++ "/MyMixer"
#else
       getAppUserDataDirectory "MyMixer"
#endif

getDBPath :: T.Text -> IO T.Text
getDBPath dbName = do
  confDir <- getConfDir
  createDirectoryIfMissing True confDir

  return $ T.concat [T.pack confDir, "/", dbName]
