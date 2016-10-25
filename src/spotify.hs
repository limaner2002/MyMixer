{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

import ClassyPrelude
import Prelude ()
import Transient.Base
import Database.Persist.Sqlite
import Control.Monad.Base

import McScraper
import NprScraper
import RPScraper
import Mixer
import Core (dbLocation, migrateAll)

data CommandOptions
    = Mc
    | Mix
    | Npr
    | Migrate
    | Rp
    | Test
      deriving (Show, Eq, Read, Typeable)

-- main :: IO ()
-- main = do
--   selectedOp <- keep $ do
--     r <-        option Mc "Scrape tracks from the Music Choice website."
--             <|> option Npr "Scrape tracks from All Things Considered on NPR."
--             <|> option Rp "Scrape tracks from RadioParadise."
--             <|> option Mix "Take the tracks from the SQLite database and create a mix from them."
--             <|> option Test "Run the test function."
--             <|> option Migrate "Run the sqlite migration."
--     exit r

--   case selectedOp of
--     Mc -> keep mcScraper
--     Npr -> keep $ oneThread nprScraper
--     Rp -> keep $ oneThread rpScraper
--     Mix -> (runSqlite dbLocation $ findTracks :: IO ())
--     Test -> testFcn
--     Migrate -> runSqlite dbLocation $ runMigration migrateAll

main = runSqlite dbLocation findTracks

testFcn :: (MonadIO m, MonadBase IO m) => m ()
testFcn = do
  putStrLn "Please enter (Y/N)"
  (answer :: Text) <- getLine
  case answer of
    "Y" -> putStrLn "You chose no!"
    "N" -> putStrLn "You chose yes!"
