{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module McScraper
    ( mcScraper
    ) where

import ClassyPrelude
import Prelude (iterate)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Transient.Base
import Transient.Indeterminism
import Transient.EVars
import Control.Applicative ((<|>))
import System.Random
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as CC
import Data.Aeson
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Control.Monad.Loops
import Control.Exception
import Database.Persist.Sqlite (runSqlite, Key, runMigration)

import Core

mcScraper :: TransIO ()
mcScraper = do
  mgr <- liftIO $ newManager tlsManagerSettings
  var <- newEVar
  liftIO $ runSqlite dbLocation $ runMigration migrateAll

  -- (track, stationId) <- foldr (<|>) empty $ fmap (getTrackStation mgr . StationKey) [44, 3]
  scrapeStations mgr var <|> (threads 1 $ saveStationTrack var)

scrapeStations :: Manager -> EVar (Track, Key Station) -> TransIO ()
scrapeStations mgr var = foldr (<|>) empty $ fmap (getTrackStation mgr var . StationKey) [44,3,117,2,4,6,14,22,27,32,35,36,38,39,40,47,48,150]

saveStationTrack :: EVar (Track, Key Station) -> TransIO ()
saveStationTrack var = do
  (track, stationId) <- readEVar var
  liftIO $ do
    id <- myThreadId
    putStrLn $ pprintTrack track

    runSqlite dbLocation $ addToDB track stationId
    
       

getTrackStation :: Manager -> EVar (Track, Key Station) -> Key Station -> TransIO ()
getTrackStation mgr var stationId = do
  track <- iterateEvents' (getTrack mgr (baseURL <> show (unStationKey stationId))) 0
  writeEVar var (track, stationId)

getTrack :: Manager -> String -> Int -> IO (Track, Int)
getTrack mgr url delay = do
  threadDelay (delay * 1000)
  mVal <- worker mgr url
  case mVal of
    Nothing -> error $ "Could not access the url: " <> url
    Just val ->
        case val of
          Left msg -> error $ unpack msg
          Right (MCTrack tup) -> return tup

mul :: Int
mul = truncate 1e6

worker :: FromJSON a => Manager -> String -> IO (Maybe (Either Text a))
worker mgr url = do
  request <- liftIO $ parseRequest url
  res <- runResourceT $ httpRequest mgr request
  return res

decodeJSON :: FromJSON a => MonadResource m => ConduitM (PositionRange, Value) (Either Text a) m ()
decodeJSON = CC.map (\(_, x) -> case fromJSON x of
                                         Error msg -> Left $ pack msg
                                         Success val -> Right val
                    )

httpRequest :: MonadResource m => FromJSON a => Manager -> Request -> m (Maybe (Either Text a))
httpRequest mgr req = do
  response <- http req mgr
  responseBody response $$+- conduitParser json =$ decodeJSON =$ CC.last

baseURL :: Textual t => t
baseURL = "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/"

newtype MCTrack = MCTrack (Track, Int)
    deriving Show

instance FromJSON MCTrack where
    parseJSON (Object o) = MCTrack <$> tuple
        where
          track = Track <$> o .: "Line1"
                        <*> o .: "Line2"
                        <*> o .: "Line3"
                        <*> pure Nothing
          ttl = o .: "TimeToLive"
          tuple = (,) <$> track <*> ttl
           
iterateEvents' :: (a -> IO (b, a)) -> a -> TransIO b
iterateEvents' f init = go init
    where
      go x = do
        (output, feedback) <- async (f x)
        return output <** go feedback
