{-# LANGUAGE OverloadedStrings #-}
module HTTPClient ( module HTTPClient
                  , Network.HTTP.Conduit.tlsManagerSettings
                  , Network.HTTP.Conduit.newManager
                  , Network.HTTP.Conduit.Manager
                  , Control.Monad.Trans.Resource.runResourceT
                  ) where

import Network.HTTP.Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)

import Types

dbLock :: TMVar Int
dbLock = unsafePerformIO $ newTMVarIO 1

getStationInfo :: Manager -> Int -> Scraper ()
getStationInfo mgr stationID = do
  req <- parseUrl ("http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/" ++ show stationID)
  res <- httpLbs req mgr
  let t = decode (responseBody res) :: Maybe Track
  case t of
    Nothing -> liftIO $ putStrLn "Could not get track information"
    Just track -> do
           liftIO $ putStrLn $ "Station ID: " ++ show stationID
           liftIO $ putStrLn $ show track ++ "\n"
           lock <- liftIO $ atomically $ takeTMVar dbLock
           insertIfDifferent (track { trackStation = stationID })
           transactionSave
           liftIO $ atomically $ putTMVar dbLock lock
           put $ Just track
      
insertIfDifferent :: Track -> Scraper ()
insertIfDifferent track = do
  lastTrack <- get
  case lastTrack of
    Nothing -> saveTrack track
    Just lastTrack ->
        if lastTrack == track
        then return ()
        else saveTrack track