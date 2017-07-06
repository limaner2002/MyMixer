{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RPScraper where

import ClassyPrelude

import Control.Arrow
import Database.Persist.Sqlite (runSqlite, SqlBackend)
import qualified Streaming.Prelude as S
import Text.Taggy.Lens
import Control.Lens hiding (children)
import Data.List.Split (chunksOf)
import qualified Data.Text.Lazy as TL
import Data.ByteString.Streaming.HTTP hiding (withHTTP)

import Core

rpScraper :: (MonadBaseControl IO m, MonadIO m) => Text -> m ()
rpScraper dbLocation = runSqlite dbLocation rpScraper'

rpScraper' :: (MonadIO m, MonadBaseControl IO m) => ReaderT SqlBackend m ()
rpScraper' = do
  tracks <- liftIO $ getTrackInfo
  makeTracks >>> S.each >>> S.concat >>> addTracks $ tracks

addTracks :: MonadIO m =>
     S.Stream (S.Of Track) (ReaderT SqlBackend m) r
     -> ReaderT SqlBackend m r
addTracks = S.mapM_ (flip addToDB (StationKey 1000))

createTrack :: [Text] -> Either String Track
createTrack [artist, name, album] = Right $ Track artist name (Just album) Nothing
createTrack l = Left "Incorrect input"

createTracks = fmap createTrack . chunksOf 3

rpURL = "http://www.radioparadise.com/rp2-content.php?name=Playlist&more=true"

getTrackInfo :: IO TL.Text
getTrackInfo = decodeUtf8 . responseBody <$> (join $ httpLbs <$> parseRequest rpURL <*> newManager tlsManagerSettings)

parseTrackInfo :: TL.Text -> [[Text]]
parseTrackInfo x = chunksOf 4 $ x ^.. html . taking 1 (dropping 7 $ allNamed (only "table")) . allNamed (only "a") . children . folded . runFold (Fold content <|> Fold contents)

makeTracks :: TL.Text -> [Either String Track]
makeTracks = parseTrackInfo >>> fmap (take 3) >>> fmap createTrack
