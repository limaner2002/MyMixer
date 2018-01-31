{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ScrapeITunes where

import ClassyPrelude
import ITunes
import Internal.Database
import Internal.Types
import Internal.Newtypes
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Servant.Client
import ScrapeCommon

readPlaylist :: FilePath -> IO (Result [ITunesTrack])
readPlaylist fp = do
    let asValue = to (id :: Value -> Value)          
    x <- readFile fp
    pure $ sequence $ x ^.. _JSON . asValue . key "data" . plate . key "relationships" . key "tracks" . key "data" . plate . key "attributes" . to fromJSON

insertITunesTracks :: (MonadDatabase m, MonadIO m) => [ITunesTrack] -> m ()
-- insertITunesTracks (Error error) = putStrLn $ pack error
-- insertITunesTracks (Success iTunesTracks) = insert tracks' "track"
insertITunesTracks iTunesTracks = insert tracks' "track"
   where
       tracks = iTunesTracks ^.. traverse . unITunes
       tracks' = fmap (\(t, i) -> trackId .~ (Just $ TrackId i) $ t) $ zip tracks [0..]

scrapeITunes :: [PlaylistId] -> JWT -> IO ()
scrapeITunes pids tok = do
    env <- clientEnv "api.music.apple.com"
    res <- runClientM (playlists pids tok) env
    case res of
        Left err -> print err
        Right v -> do
              dbRes <- runDatabaseTIO $ insertITunesTracks $ v ^.. to _data . traverse . resRelationships . plTracks . trData . traverse . trdAttributes
              case dbRes of
                   Left err -> print err
                   Right v -> return v
