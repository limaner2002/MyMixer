module Util where

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
