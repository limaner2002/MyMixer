module NprScraper where

import Prelude ()
import ClassyPrelude

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Core
import Control.Arrow
import Database.Persist.Sqlite (runSqlite)
import Transient.Base
import Text.XML.HXT.HTTP

import Core

nprScraper :: TransIO ()
nprScraper = do
  tracks <- liftIO $ runX $ readDocument [withValidate no, withParseHTML yes, withWarnings no, withHTTP mempty] "http://www.npr.org/programs/all-things-considered/" //> hasAttrValue "class" (== "song-meta-wrap") >>> (getIt "song-meta-title" &&& getIt "song-meta-artist") >>> arr createTrack
  liftIO $ runSqlite dbLocation $ mapM_ (\x -> addToDB x (StationKey 1001)) tracks
  putStrLn $ "Scraped " <> tshow (length tracks) <> " tracks from \"All Things Considered.\""

getIt val = getChildren >>> hasAttrValue "class" (== val) >>> getChildren >>> getText

createTrack (title, artist) = Track (pack artist) (pack title) Nothing Nothing
