module Options
    ( Command(..),
      parseArgs
    )
    where

import Options.Applicative
import Resources

data Command
    = TmpAdd FilePath
    | ReadPlaylists FilePath
    | WritePlaylists FilePath
    | Search FilePath
    | LocalSearch DirectoryPath

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseTmpAdd :: Parser Command
parseTmpAdd = TmpAdd
              <$> argument str (metavar "FILE-PATH")

parseReadPlaylists :: Parser Command
parseReadPlaylists = ReadPlaylists
                     <$> argument str (metavar "DATABASE-FILE")

parseWritePlaylists :: Parser Command
parseWritePlaylists = WritePlaylists
                      <$> argument str (metavar "DATABASE-FILE")

parseSearch :: Parser Command
parseSearch = Search
              <$> argument str (metavar "TRACK-FILE")

parseLocalSearch :: Parser Command
parseLocalSearch = LocalSearch
                   <$> argument str (metavar "LOCAL-DIRECTORY")

parseArgs :: Parser Command
parseArgs = subparser $
            command "tmpAdd" (parseTmpAdd `withInfo` "Add tracks to the tmp playlist") <>
            command "readPlaylists" (parseReadPlaylists `withInfo` "Update the source playlists from local files.") <>
            command "writePlaylists" (parseWritePlaylists `withInfo` "Update the local files from the source playlists.") <>
            command "search" (parseSearch `withInfo` "Search spotify's library for the tracks contained in TRACK-FILE. The file should contain one track per line each tab delimited with the first column the name of the artist and the second column the name of the track") <>
            command "localSearch" (parseLocalSearch `withInfo` "Search in LOCAL-DIRECTORY for songs in your collection.")
