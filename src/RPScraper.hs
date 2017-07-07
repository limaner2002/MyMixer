{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RPScraper where

import ClassyPrelude

import Control.Arrow
import Database.Persist.Sqlite (runSqlite, SqlBackend)
import qualified Streaming.Prelude as S
import Text.Taggy.Lens
import Control.Lens hiding (children, elements)
import Data.List.Split (chunksOf)
import qualified Data.Text.Lazy as TL
import Data.ByteString.Streaming.HTTP hiding (withHTTP)

import Core

rpScraper :: (MonadBaseControl IO m, MonadIO m) => Text -> m ()
rpScraper dbLocation = runSqlite dbLocation rpScraper'

rpScraper' :: (MonadIO m, MonadBaseControl IO m) => ReaderT SqlBackend m ()
rpScraper' = do
  tracks <- liftIO $ getTrackInfo
  parseTrackInfo >>> S.each >>> addTracks $ tracks

addTracks :: MonadIO m =>
     S.Stream (S.Of Track) (ReaderT SqlBackend m) r
     -> ReaderT SqlBackend m r
addTracks = S.mapM_ (flip addToDB (StationKey 1000))

rpURL = "http://www.radioparadise.com/rp2-content.php?name=Playlist&more=true"

getTrackInfo :: IO TL.Text
getTrackInfo = decodeUtf8 . responseBody <$> (join $ httpLbs <$> parseRequest rpURL <*> newManager tlsManagerSettings)

parseTrackInfo :: TL.Text -> [Track]
parseTrackInfo x = x ^.. html . taking 1 (dropping 7 $ allNamed (only "table")) . allNamed (only "tr") . runFold (Track <$> getArtist <*> getTrack <*> (Just <$> getAlbum) <*> pure Nothing)

getArtist :: HasElements s => ReifiedFold s Text
getArtist = Fold (elements . elements . contents)

getTrack :: HasElements s => ReifiedFold s Text
getTrack = Fold (elements . elements . elements . contents)

getAlbum :: HasElements s => ReifiedFold s Text
getAlbum = Fold (taking 1 $ elements . elements . elements . elements . elements . elements . contents)
