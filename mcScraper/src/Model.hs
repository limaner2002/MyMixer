{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model ( Data.Aeson.decode
             , Track (..)
             , Database.Persist.Sqlite.runSqlite
             , Scraper
             , Database.Persist.Sql.runMigration
             , Database.Persist.Sql.transactionSave
             , migrateAll
             , getStations
             , renderStations
             , saveTrack
             , Control.Monad.State.evalStateT
             , Control.Monad.State.get
             , Control.Monad.State.put 
             )where

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist
import Data.Aeson
import Control.Monad.State
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Logger (NoLoggingT, LoggingT, logInfoN)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Tabular

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Track
  artist String
  name String
  album String
  station Int
Station
  name String
  idNum Int
  Primary idNum
|]

instance FromJSON Track where
    parseJSON (Object o) = Track <$>
                           o .: "Line1" <*>
                           o .: "Line2" <*>
                           o .: "Line3" <*>
                           o .:? "stationID" .!= 0

instance Show Track where
    show (Track ar nm al _) =
        "Artist: " ++ ar ++ "\n" ++
        "Name: " ++ nm ++ "\n" ++
        "Album: " ++ al

instance Eq Track where
    (Track artist1 name1 album1 _) == (Track artist2 name2 album2 _) =
        artist1 == artist2 &&
        name1 == name2 &&
        album1 == album2

type Scraper = SqlPersistT (NoLoggingT (ResourceT (StateT (Maybe Track) IO)))

saveTrack :: Track -> Scraper ()
saveTrack track =
    insert_ track

getStations :: Scraper [Station]
getStations = do
  ents <- selectList [] [Asc StationName]
  return $ fmap entityVal ents

renderStations :: [Station] -> String
renderStations stations =
    renderRows $ fmap
                   ( \(Station name id) ->
                         [name, show id]
                   ) stations