{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Spotify where

import ClassyPrelude
import Servant.Client
import Servant.API
import Internal.Types
import Data.Proxy
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Retry
import Internal.Database
import ScrapeCommon
import Network.HTTP.Types (statusCode)
import Control.Monad.Except (MonadError, catchError, throwError)

type TrackByIsrc = "v1" :> "search" :> QueryParam "q" ByIsrc :> QueryParam "type" TrackType :> Header "Authorization" AuthToken :> QueryParam "limit" Int :> Get '[JSON] Value
type TokenRequest = "api" :> "token" :> Get '[JSON] Value

newtype ByIsrc = ByIsrc Isrc
    deriving Show

instance ToHttpApiData ByIsrc where
    toQueryParam (ByIsrc (Isrc isrc)) = "isrc:" <> isrc

data TrackType = TrackType

instance ToHttpApiData TrackType where
    toQueryParam _ = "track"

newtype AuthToken = AuthToken Text
    deriving (Show, Eq, IsString)

instance ToHttpApiData AuthToken where
    toHeader (AuthToken tok) = "Bearer " <> encodeUtf8 tok
    toUrlPiece (AuthToken tok) = toUrlPiece tok

newtype SpotifyUri = SpotifyUri { unSpotifyUri :: Text }
  deriving (Show, Eq)

trackByIsrc :: Isrc -> AuthToken -> Maybe Int -> ClientM Value
trackByIsrc isrc tok mLimit = client (Proxy :: Proxy TrackByIsrc) (Just $ ByIsrc isrc) (Just TrackType) (Just tok) mLimit

searchTrack :: Isrc -> AuthToken -> ClientM (Maybe SpotifyUri)
searchTrack isrc tok = do
  searchResult <- retryingC $ trackByIsrc isrc tok (Just 1)
  return $ searchResult ^? key "tracks" . key "items" . plate . key "uri" . _String . to SpotifyUri

searchTracks :: [Isrc] -> AuthToken -> ClientM [SpotifyUri]
searchTracks isrcs tok = do
  mUris <- mapM (flip searchTrack tok) isrcs
  return $ mUris ^.. traverse . _Just

searchByISRC :: AuthToken -> IO ()
searchByISRC tok = do
  putStrLn "Retrieving tracks from database"
  tracks <- runDatabaseTIO retrieveTrack

  let isrcs = tracks ^.. _Right . _Right . traverse . _Right . isrc

  putStrLn "Creating env"
  env <- clientEnv "api.spotify.com"

  putStrLn "Searching for tracks"
  eSpotifyURIS <- runClientM (searchTracks isrcs tok) env
  case eSpotifyURIS of
    Left err -> print err
    Right uris -> do
      putStrLn "Writing tracks to file"
      writeFile "/tmp/tracks.txt" $ encodeUtf8 $ intercalate "\n" $ fmap unSpotifyUri uris
  putStrLn "Done"

retryingC :: (MonadError ServantError m, MonadIO m) => m a -> m a
retryingC f = do
  res <- retrying searchRetryPolicy shouldRetry (const ((Right <$> f) `catchError` (pure . Left)))
  either throwError pure res

searchRetryPolicy :: Monad m => RetryPolicyM m
searchRetryPolicy = exponentialBackoff 1000000 `mappend` limitRetries 10

shouldRetry :: Monad m => RetryStatus -> (Either ServantError a) -> m Bool
shouldRetry _ (Left (FailureResponse resp)) = pure (code == 429 || code == 502)
  where
    code = statusCode (responseStatusCode resp)
shouldRetry _ _ = pure False
