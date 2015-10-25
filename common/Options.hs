module Options where

import Options.Applicative

data Command = FindTracks Int
             | DisplayStations

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info opts $ progDesc desc

parseSearch :: Parser Command
parseSearch = FindTracks
              <$> argument auto (metavar "STATION-ID")

parseDisplay :: Parser Command
parseDisplay = pure DisplayStations

parseArgs :: Parser Command
parseArgs = subparser $
            command "search" (parseSearch `withInfo` "Search spotify's library for the tracks contained in TRACK-FILE. The file should contain one track per line each tab delimited with the first column the name of the artist and the second column the name of the track")
         <> command "list-stations" (parseDisplay `withInfo` "List available stations")