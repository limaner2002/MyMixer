{-# LANGUAGE OverloadedStrings #-}

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Time
import Control.Monad.Loops (whileM_)
import Control.Monad.IO.Class

import Tabular
import HTTPClient
import Util
import Model

delay = truncate 30e6

forkChild :: IO () -> IO ThreadId
forkChild io = do
  tid <- myThreadId
  putStrLn $ "My tid is " ++ show tid
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

mainLoop :: Manager -> Int -> Int -> Scraper ()
mainLoop mgr stationID maxTime = do
  startTime <- liftIO $ getCurrentTime
  whileM_ ( do
            now <- liftIO $ getCurrentTime
            return $ (diffUTCTime now startTime) < (fromIntegral maxTime)
          )
          ( do
            getStationInfo mgr stationID
            liftIO $ threadDelay delay
          )

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  db <- getDBPath "mc.sqlite"

  putStrLn "How long do you want this to run?"
  line <- getLine
  let maxTime = read line

  stations <- evalStateT ( runSqlite db $ do
                             runMigration migrateAll
                             getStations
                         ) Nothing

  putStrLn "Stations:"
  putStrLn $ renderStations stations
  putStrLn "Enter the station ids separated by spaces"
  line <- getLine
  let args = splitOn " " line
      ids = map read args

  mapM_ (\id ->
         forkChild
           (evalStateT (runSqlite db $ mainLoop mgr id maxTime) Nothing)
        ) ids
  waitForChildren
