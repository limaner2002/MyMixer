import System.IO.Error (tryIOError, IOError)
import System.Environment
import Data.Conduit
import System.FilePath ((</>))
import System.Directory
import Control.Monad.Trans.Class (lift)
import Control.Monad (filterM, forM_)

import Control.Monad.Trans.Resource (runResourceT)
import TagReader

type Ext = String

data DirContent = DirList [FilePath] [FilePath]
                | DirError IOError

data DirData = DirData FilePath DirContent

-- Gets the contents of the 'path' and recursively walks through all
-- subdirectories.
walk :: FilePath -> Source IO DirData
walk path = do
  result <- lift $ tryIOError listDir
  case result of
    Left error -> yield $ DirData path $ DirError error
    Right dir@(DirList subdirs contents) -> do
                yield $ DirData path dir
                forM_ (subdirs) (walk . (path </>))
 where
   listDir = do
             entries <- getDirectoryContents path
             subdirs <- filterM isDir entries >>= filterHidden
             return $ DirList subdirs entries
   filterHidden paths = return $ filter (\x -> head x /= '.') paths
   isDir entry = doesDirectoryExist (path </> entry)

-- Gets the filtered contents of the directory walk and passes the
-- full path to the tag reader.
displayContents :: Sink DirData IO ()
displayContents = addCleanup (\_ -> putStrLn "Finished.") $ loop 1
    where
      loop n = do
        lift $ putStr $ ">> " ++ show n ++ ". "
        r <- await
        case r of
          Nothing -> return ()
          Just r -> do
                 lift (process r) >> loop (n + 1)
      process (DirData path (DirList subdirs contents)) = do
        putStrLn $ "Visiting directory: " ++ path
        let fullPaths = map ((path ++ "/") ++ ) contents
        mapM_ (\path -> do
                        putStrLn $ "Opening " ++ path
                        runResourceT $ readTag path) fullPaths

-- Filters the directory contents to only include those with the
-- '.mp3' extension. Support for other file formats is not expected
-- any time soon.
getMusicFiles :: Conduit DirData IO DirData
getMusicFiles = do
  r <- await
  case r of
    Nothing -> return ()
    Just (DirData path (DirList subdirs contents)) -> do
                  let musicFiles = filter (\x -> hasExtension x ".mp3") contents
                  yield $ DirData path $ DirList subdirs musicFiles
                  getMusicFiles

-- Utility functions
hasExtension :: FilePath -> Ext -> Bool
hasExtension path ext = and $ zipWith (==) (reverse path) (reverse ext)

main = do
  (path:args) <- getArgs
  walk path $$ getMusicFiles =$ displayContents
