{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Opts where

import ClassyPrelude
import MixerProcess
import MCScraper
import Options.Applicative
import RPScraper

mixerInfo :: ParserInfo (IO ())
mixerInfo = info (helper <*> mixerOptParser)
  (  fullDesc
  <> header "Mixer"
  <> progDesc "Mixes tracks from various sources using weighted probabilities."
  )

mixerOptParser :: Parser (IO ())
mixerOptParser = runIt
  <$> (pack <$> strOption
  (  long "dbPath"
  <> short 'p'
  <> metavar "DB_PATH"
  <> help "The path of the sqlite database."
  ))
  <*> strOption
  (  long "weightPath"
  <> short 'w'
  <> metavar "WEIGHT_PATH"
  <> help "The path to the file that contains the station weights."
  )
  <*> option auto
  (  long "nStations"
  <> short 'n'
  <> metavar "N_STATIONS"
  <> help "The number of stations to sample from without replacement."
  )
  <*> (encodeUtf8 . pack
       <$> (strOption
             (  long "authToken"
             <> short 't'
             <> metavar "AUTH_TOKEN"
             <> help "The oauth token to use."
             )
           )
      )

mcScraperInfo :: ParserInfo (IO ())
mcScraperInfo = info (helper <*> mcScraperOptParser)
  (  fullDesc
  <> header "MCScraper"
  <> progDesc "Scrapes tracks from Music Choice."
  )

mcScraperOptParser :: Parser (IO ())
mcScraperOptParser = mcScraper
  <$> dbPathParser
  <*> option auto
  (  long "threshold"
  <> short 't'
  <> metavar "THRESHOLD"
  <> help "The threshold below which stations will be scraped."
  )
  <*> option auto
  (  long "stopAt"
  <> short 's'
  <> metavar "STOP_AT"
  <> help "When scraping tracks for a station, continue doing so until STOP_AT tracks have been scraped"
  )

rpScraperInfo :: ParserInfo (IO ())
rpScraperInfo = info (helper <*> rpScraperOptParser)
  (  fullDesc
  <> header "RPScraper"
  <> progDesc "Scrapes tracks from Radio Paradise."
  )

rpScraperOptParser :: Parser (IO ())
rpScraperOptParser = rpScraper
  <$> dbPathParser

dbPathParser :: Parser Text
dbPathParser = pack <$> strOption
      (  long "dbPath"
      <> short 'p'
      <> metavar "DB_PATH"
      <> help "The path to the sqlite database."
      )
