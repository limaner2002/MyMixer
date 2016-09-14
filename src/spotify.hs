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
import Mixer
import Core (dbLocation, migrateAll)

data CommandOptions
    = Mc
    | Mix
    | Npr
    | Migrate
    | Test
      deriving (Show, Eq, Read, Typeable)

main = keep $ do
  selectedOp <- oneThread $ option Mc "Scrape tracks from the Music Choice website."
            <|> option Npr "Scrape tracks from All Things Considered on NPR."
            <|> option Mix "Take the tracks from the SQLite database and create a mix from them."
            <|> option Test "Run the test function."
            <|> option Migrate "Run the sqlite migration."
  case selectedOp of
    Mc -> mcScraper
    Npr -> oneThread nprScraper
    Mix -> runSqlite dbLocation $ findTracks
    Test -> oneThread testFcn
    Migrate -> liftIO $ runSqlite dbLocation $ runMigration migrateAll

testFcn :: (MonadIO m, MonadBase IO m) => m ()
testFcn = do
  answer <- liftBase $ keep $ input (const True) "Yes" <|> input (const True) "No"
  case answer of
    No -> putStrLn "You chose no!"
    Yes -> putStrLn "You chose yes!"
