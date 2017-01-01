{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

import ClassyPrelude
import Prelude ()
import qualified Transient.Base as Trans
import Database.Persist.Sqlite
import Control.Monad.Base

import McScraper
import NprScraper
import RPScraper
import Mixer
import Core (dbLocation, migrateAll)
import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA

data CommandOptions
    = Mc
    | Mix
    | Npr
    | Migrate
    | Rp
    | Test
      deriving (Show, Eq, Read, Typeable)

transientMain :: IO ()
transientMain = do
  selectedOp <- Trans.keep $ do
    r <-        Trans.option Mc "Scrape tracks from the Music Choice website."
            <|> Trans.option Npr "Scrape tracks from All Things Considered on NPR."
            <|> Trans.option Rp "Scrape tracks from RadioParadise."
            <|> Trans.option Mix "Take the tracks from the SQLite database and create a mix from them."
            <|> Trans.option Test "Run the test function."
            <|> Trans.option Migrate "Run the sqlite migration."
    Trans.exit r

  case selectedOp of
    Mc -> Trans.keep mcScraper
    Npr -> Trans.keep $ Trans.oneThread nprScraper
    Rp -> Trans.keep $ Trans.oneThread rpScraper
    Test -> testFcn
    Migrate -> runSqlite dbLocation $ runMigration migrateAll

data Options
  = Scraper
  | Mixer FilePath Int Int Int
  deriving (Show, Eq, Read)

optionsParser :: Parser Options
optionsParser = scraperParser <|> mixerParser

mixerParser :: Parser Options
mixerParser = Mixer
  <$> strOption
    ( long "config"
    OA.<> metavar "CONFIG_FILE_PATH"
    OA.<> help "The filepath of the config file containing the station names, weights, and ids."
    )
  <*> option auto
    (  long "stations"
    OA.<> metavar "N_STATIONS"
    OA.<> help "The number of stations to pull from before replacing."
    )
  <*> option auto
    (  long "tracks"
    OA.<> metavar "N_TRACKS"
    OA.<> help "The number of tracks to select from each station."
    )
  <*> option auto
    (  short 'n'
    OA.<> metavar "N"
    OA.<> help "The number of times to repeat this process."
    )

scraperParser :: Parser Options
scraperParser = argument auto (metavar "Scraper")

optsInfo :: ParserInfo Options
optsInfo = info (helper <*> optionsParser)
  (  fullDesc
  OA.<> progDesc "Print a greeting for TARGET"
  OA.<> header "hello - a test for optparse-applicative"
  )

main :: IO ()
main = do
  opt <- execParser optsInfo
  case opt of
    Scraper -> transientMain
    (Mixer configFilePath x y z) -> runSqlite dbLocation (findTracks configFilePath x y z)

testFcn :: (MonadIO m, MonadBase IO m) => m ()
testFcn = do
  putStrLn "Please enter (Y/N)"
  (answer :: Text) <- getLine
  case answer of
    "Y" -> putStrLn "You chose no!"
    "N" -> putStrLn "You chose yes!"
