{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

import ClassyPrelude
import Opts
import Options.Applicative

import MachineUtils
import qualified System.IO as SIO

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  join $ execParser appInfo

appParser :: Parser (IO ())
appParser = subparser
  (  command "mixer" mixerInfo
  <> command "mcScraper" mcScraperInfo
  <> command "rpScraper" rpScraperInfo
  )

appInfo :: ParserInfo (IO ())
appInfo = info (helper <*> appParser)
  (  fullDesc
  <> progDesc "My mixer program"
  )
