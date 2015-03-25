module Util
    where

import System.IO
import Data.List.Split
import System.FilePath
import Data.Aeson
import Data.Monoid
import Network.OAuth2.OAuth2
import Network.OAuth2.Util
import Resources

fetchSeveral :: (FromJSON a, Monoid a) => [String] -> Flow (a)
fetchSeveral [] = return mempty
fetchSeveral (url:urls) = do
   page <- fromAuthUrl url
   next <- fetchSeveral urls
   return $ page `mappend` next

fetchPagingObjects :: (FromJSON a) => Maybe String -> Flow (PagingObject a)
fetchPagingObjects Nothing = return mempty
fetchPagingObjects (Just url) = do
  page <- fromAuthUrl url
  next <- fetchPagingObjects (next page)
  return $ page `mappend` next

getLines :: Handle -> IO [String]
getLines handle = do
  isEOF <- hIsEOF handle
  if isEOF
  then return []
  else do
    line <- hGetLine handle
    next <- getLines handle
    return $ line : filter (\x -> x /= []) next

extractId :: String -> String
extractId uri = last $ splitOn ":" uri

extractUserName :: String -> String
extractUserName uri = (splitOn ":" uri) !! 2

getURIs :: FilePath -> IO [String]
getURIs path = do
  withFile path ReadMode (\handle -> do
       		     getLines handle)

printTracks :: Maybe Tracks -> IO ()
printTracks Nothing = return ()
printTracks (Just trackList) = do
  putStrLn $ show trackList

getCurrentUser :: Flow UserObjectPrivate
getCurrentUser = fromAuthUrl "https://api.spotify.com/v1/me" :: Flow UserObjectPrivate
