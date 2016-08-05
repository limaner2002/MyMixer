{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import qualified Data.Text as T
import Text.HTML.DOM (readFile)
import Text.XML.Cursor (Cursor, content, element, fromDocument,
                        ($//), (&/), ($/))
import Types
import Util

-- The data we're going to search for
findNodes :: Cursor -> [[Cursor]]
findNodes cursor =
    fmap cells rows
  where
    cells row = row $// element "a"
    rows = cursor $// element "tr"

getDom = readFile "/tmp/RP.html"

-- The list that I get is as follows,
-- [[["Artist1", "Track Name1"], ["Album1"]]]
printTracks :: [[[T.Text]]] -> RPScraper ()
printTracks xs =
   mapM_ f $ fmap mconcat xs
 where
   f :: [T.Text] -> RPScraper ()
   f [artist, name, album] = do
     let t = Track artist name album 1000
     saveTrack t
   f [] = return ()
   f l = liftIO $ putStrLn $ "Could not create track.\nReceived: " ++ show l

main = do
  db <- getDBPath "db.sqlite"
  dom <- readFile "/tmp/RP.html"
  let cursor = fromDocument dom
      cells = findNodes cursor
      values = fmap (fmap (\x -> x $/ content)) cells
  runSqlite db $ printTracks values
