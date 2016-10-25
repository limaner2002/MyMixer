{-# LANGUAGE OverloadedStrings #-}

module RPScraper where

import Prelude ()
import ClassyPrelude

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Core
import Control.Arrow
import Database.Persist.Sqlite (runSqlite)
import Transient.Base
import Text.XML.HXT.HTTP
import Data.List.Split

import Core

rpScraper :: TransIO ()
rpScraper = do
  r <- liftIO $ getTracks
  let tracks = fmap createTrack $ chunksOf 3 $ fmap pack r
  liftIO $ runSqlite dbLocation $ mapM_ (\x -> addToDB x (StationKey 1000)) tracks
  putStrLn $ "Scraped " <> tshow (length tracks) <> " tracks from \"Radio Paradise.\""

createTrack :: [Text] -> Track
createTrack [artist, name, album] = Track artist name (Just album) Nothing
createTrack l = error "Incorrect input"

createTracks = fmap createTrack . chunksOf 3 . fmap pack

getTracks =
  runX $ (readDocument [withValidate no, withParseHTML yes, withHTTP mempty] rpURL //> hasName "table")
        >>. (take 1 . drop 3)
        /> hasName "tr"
        /> hasName "td"
        /> hasName "a"
        /> getText

rpURL = "http://www.radioparadise.com/rp2-content.php?name=Playlist&more=true"
