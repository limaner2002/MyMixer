{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Util where

import System.Directory ( createDirectoryIfMissing
                        , getHomeDirectory
                        , getAppUserDataDirectory
                        )
import qualified Data.Text as T

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
