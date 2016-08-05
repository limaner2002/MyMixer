{-# LANGUAGE OverloadedStrings #-}
import Prelude ()
import ClassyPrelude hiding (readFile, Element)

import Text.HTML.DOM (readFile, parseLBS)
import Text.XML.Cursor (Cursor, content, element, fromDocument, attributeIs,
                        ($//), (&/), ($/), node, fromNode)
import Text.XML hiding (readFile, parseLBS)
import Network.HTTP.Conduit (simpleHttp)

import Types
import Util

getDocument :: IO Cursor
getDocument = do
  siteContent <- simpleHttp "http://www.npr.org/programs/all-things-considered/"
  let dom = parseLBS siteContent

  return $ fromDocument dom

nodeElement :: Node -> Element
nodeElement (NodeElement e) = e

getSongNodes :: Cursor -> [[Node]]
getSongNodes cursor = fmap join $ (fmap . fmap) (songInfo) songs
    where
      songMeta = cursor $// attributeIs "class" "song-meta-wrap"
      spans x = x $// element "span"
      songInfo = elementNodes . nodeElement . node
      songs = fmap spans songMeta

getSongText :: [[Node]] -> [[Text]]
getSongText nodes = fmap join $ (fmap . fmap) (content . fromNode) nodes

createTracks :: [[Text]] -> [Either Text Track]
createTracks trackInfo = fmap newTrack trackInfo
    where
      newTrack [name, artist] = Right $ Track artist name mempty 1001
      newTrack infos = Left $ "Could not create track from: " <> tshow infos

main :: IO ()
main = do
  db <- getDBPath "db.sqlite"
  cursor <- getDocument
  let songInfo = (getSongText . getSongNodes) cursor
      tracks = createTracks songInfo

  runSqlite db $ (mapM_ . mapM_) saveTrack tracks