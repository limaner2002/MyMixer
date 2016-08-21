{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Flow
    ( Expression (..)
    , execute
    , parseURI
    , Flow
    , SpotifyItem
    ) where

import ClassyPrelude
import Prelude ()

import ConfigFile
import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Control.Monad.Trans.Resource
import Data.Time
import Debug.Trace

readOAuth2 :: MonadThrow m => Config -> m OAuth2
readOAuth2 cfg =
    OAuth2 <$> getVal' "clientId"
           <*> getVal' "clientSecret"
           <*> getVal' "authorizeEndpoint"
           <*> getVal' "accessTokenEndpoint"
           <*> getVal' "callback"


  where
    getVal' k = getVal k cfg

readScopes :: (IsString string, MonadThrow m, Read string, Typeable string) => Config -> m [string]
readScopes cfg = getVal "scopes" cfg

data RefreshToken = RefreshToken ByteString
      deriving (Show, Read, Eq)

instance Monoid RefreshToken where
    mempty = RefreshToken mempty
    RefreshToken a `mappend` RefreshToken b = RefreshToken (a <> b)

newtype ID = ID Text
    deriving (Show, Read, Eq)

newtype User = User Text
    deriving (Show, Read, Eq)

data Artist = Artist
    { artistName :: Text
    } deriving (Show, Eq, Read)

instance FromJSON Artist where
    parseJSON (Object o) =
        Artist <$> o .: "name"

data Track = Track
    { trackArtist :: [Artist]
    , trackName :: Text
    , trackID :: ID
    } | TrackNotLoaded ID
      | TrackList [Track]
  deriving (Show, Eq, Read)

instance FromJSON Track where
    parseJSON (Object o) =
        case lookup "tracks" o of
          Nothing ->
              Track <$> o .: "artists"
                    <*> o .: "name"
                    <*> (ID <$> o .: "id")
          Just _ -> trace "Creating TrackList!" $
              TrackList <$> o .: "tracks"
    parseJSON (Array arr) = TrackList <$> toList <$> mapM (\(Object o) -> o .: "track") arr
    parseJSON _ = empty

data Playlist
    = Playlist
      { playlistName :: Text
      , playlistID :: ID
      , playlistTrackObjects :: Track
      }
    | PlaylistList [Playlist]
    | PlaylistNotLoaded ID User
  deriving (Show, Eq, Read)

instance FromJSON Playlist where
    parseJSON (Object o) =
        Playlist <$> o .: "name"
                 <*> (ID <$> o .: "id")
                 <*> (join $ (.: "items") <$> (o .: "tracks"))
    parseJSON _ = empty

data FetchTokenError = FetchTokenError Text
    deriving Typeable

instance Show FetchTokenError where
    show (FetchTokenError msg) = "FetchTokenError: " <> show msg

instance Exception FetchTokenError

data OAuthError
    = OAuthError Text
    | NotImplemented Text
    | AccessTokenLoadError Text
    deriving Typeable

instance Show OAuthError where
    show (OAuthError msg) = "OAuthError: " <> show msg
    show (NotImplemented name) = "The feature " <> show name <> " is not implemented yet."

instance Exception OAuthError

data SpotifyItem
    = ArtistF ID
    | TrackF Track
    | PlaylistF Playlist
    | SearchTerm ByteString
      deriving (Show, Eq, Read)

data Flow = Flow
    { flowToken :: AccessToken
    , flowManager :: Manager
    , flowExpiration :: Maybe UTCTime
    }

handleOAuth2Result :: MonadThrow m => OAuth2Result a -> m a
handleOAuth2Result (Left msg) = throwM $ OAuthError $ decodeUtf8 $ BL.toStrict msg
handleOAuth2Result (Right result) = return result

addSpotifyItem :: MonadThrow m => SpotifyItem -> SpotifyItem -> m SpotifyItem
addSpotifyItem (TrackF t) (PlaylistF pid) = throwM $ NotImplemented "addSpotifyItem"
addSpotifyItem _ _ = error "Cannot add these!"

fetchSpotifyItem :: (MonadThrow m, MonadIO m) => Flow -> SpotifyItem -> m SpotifyItem
fetchSpotifyItem (Flow tok mgr _) (TrackF tracks) = do
  oauth2Result <- liftIO $ authGetJSON mgr tok $ fetchTracksURI tracks
  result <- handleOAuth2Result oauth2Result
  return $ TrackF result
fetchSpotifyItem (Flow tok mgr _) (PlaylistF playlist) = do
  oauth2Result <- liftIO $ authGetJSON mgr tok $ fetchPlaylistURI playlist
  result <- handleOAuth2Result oauth2Result
  return $ PlaylistF result
fetchSpotifyItem _ _ = throwM $ NotImplemented "fetchSpotifyItem" 

putSpotifyItem :: (MonadThrow m, MonadIO m) => Flow -> SpotifyItem -> m SpotifyItem
putSpotifyItem _ _ = throwM $ NotImplemented "putSpotifyItem" 

loadSpotifyItem :: (MonadThrow m, MonadIO m) => SpotifyItem -> m SpotifyItem
loadSpotifyItem _ = throwM $ NotImplemented "loadSpotifyItem"

saveSpotifyItem :: (MonadThrow m, MonadIO m) => SpotifyItem -> m SpotifyItem
saveSpotifyItem _ = throwM $ NotImplemented "saveSpotifyItem"

searchSpotifyItem :: (MonadThrow m, MonadIO m) => Flow -> SpotifyItem -> m SpotifyItem
searchSpotifyItem _ _ = throwM $ NotImplemented "searchSpotifyItem"

removeSpotifyItem :: MonadThrow m => SpotifyItem -> SpotifyItem -> m SpotifyItem
removeSpotifyItem _ _ = throwM $ NotImplemented "removeSpotifyItem"

-- data Expression
--     = FlowAtom SpotifyItem
--     | Get Expression
--     | Put Expression
--     | Load FilePath Expression
--     | Save FilePath Expression
--     | Search Expression
--     | Add Expression Expression
--     | Remove Expression Expression
--       deriving (Show, Eq, Read)

data Expression a where
    FlowAtom :: a
    Put :: (MonadThrow m, MonadIO m) => a -> m ()
    Load :: (MonadThrow m, MonadIO m) => FilePath -> a
    Save :: (MonadThrow m, MonadIO m) => FilePath -> a -> m ()

execute :: (MonadThrow m, MonadBaseControl IO m, MonadIO m)
           => Maybe Flow
           -> Expression
           -> m (Flow, Either SomeException SpotifyItem)
execute Nothing expr = do
  mgr <- liftIO $ newManager tlsManagerSettings
  mRefreshToken <- tryAny $ loadToken "token.txt"

  tok' <- case mRefreshToken of
    Left _ -> getNewToken mgr
    Right Nothing -> getNewToken mgr
    Right (Just rTok) -> refreshToken_ mgr rTok

  currentTime <- liftIO getCurrentTime

  let flow = setExpiration currentTime $ Flow (trace (show tok') tok') mgr Nothing

  execute (Just flow) expr
execute (Just flow) expr = do
  checkFlow flow
  exprResult <- tryAny $ getSpotifyItem <$> (runResourceT $ runExpression flow expr)
  return (flow, exprResult)

getNewToken :: (MonadThrow m, MonadIO m) => Manager -> m AccessToken
getNewToken mgr = do
  cfg <- readConfigFile "/tmp/settings.conf"
  oauth <- readOAuth2 cfg
  scopes <- readScopes cfg

  code <- liftIO $ do
      print oauth
      putStrLn "Visit the following address and make sure something is listening on port 4242"
      print $ authorizationUrl oauth `appendQueryParam` fmap (\x -> ("scope", x)) scopes
      getLine
  
  oauth2Result <- liftIO $ fetchAccessToken mgr oauth code

  case oauth2Result of
    Left err -> throwM $ FetchTokenError $ decodeUtf8 $ BL.toStrict err
    Right tok -> return tok

checkFlow :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => Flow -> m Flow
checkFlow flow@(Flow tok mgr _) = do
  currentTime <- liftIO $ getCurrentTime
  case isExpired currentTime flow of
    False -> return flow
    True -> do
      liftIO $ putStrLn "Access token has expired. Requesting a new one."
      rTok <- loadToken "token.txt"
      mTok <- mapM (refreshToken_ mgr) rTok
      case mTok of
        Nothing -> throwM $ OAuthError "Cannot refresh the access token since no refresh token is present."
        Just tok -> do
                  currentTime <- liftIO $ getCurrentTime
                  return $ setExpiration currentTime $ flow {flowToken = tok}

refreshToken_ :: (MonadThrow m, MonadIO m) => Manager -> ByteString -> m AccessToken
refreshToken_ mgr rTok = do
  cfg <- readConfigFile "/tmp/settings.conf"
  oauth <- readOAuth2 cfg

  result <- liftIO $ fetchRefreshToken mgr oauth rTok
  handleOAuth2Result result

saveToken :: MonadIO m => FilePath -> AccessToken -> m ()
saveToken path tok = writeFile path $ tshow $ refreshToken tok

loadToken :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => FilePath -> m (Maybe ByteString)
loadToken path = do
  contents <- readFile path
  case readMay (contents :: Text) of
    Nothing -> throwM $ AccessTokenLoadError $ "Could not load access token from " <> tshow path
    Just tok -> return tok

dispScopes :: (IsString string, Semigroup string, Monoid string) => [string] -> string
dispScopes scopes = "scopes=" <> intercalate " " scopes

runExpression :: (MonadThrow m, MonadIO m) => Flow -> Expression -> m Expression
runExpression flow (FlowAtom a `Add` FlowAtom b)    = FlowAtom <$> addSpotifyItem a b
runExpression flow (expr1 `Add` expr2)              = Add <$> runExpression flow expr2 <*> runExpression flow expr2
runExpression flow (Get (FlowAtom a))               = FlowAtom <$> fetchSpotifyItem flow a
runExpression flow (Get expr)                       = Get <$> runExpression flow expr
runExpression flow (Put (FlowAtom a))               = FlowAtom <$> putSpotifyItem flow a
runExpression flow (Put expr)                       = Put <$> runExpression flow expr
runExpression flow (Load path (FlowAtom a))         = FlowAtom <$> loadSpotifyItem a
runExpression flow (Load path expr)                 = Load path <$> runExpression flow expr
runExpression flow (Save path (FlowAtom a))         = FlowAtom <$> saveSpotifyItem a
runExpression flow (Save path expr)                 = Save path <$> runExpression flow expr
runExpression flow (Search (FlowAtom a))            = FlowAtom <$> searchSpotifyItem flow a
runExpression flow (Search expr)                    = Search <$> runExpression flow expr
runExpression flow (FlowAtom a `Remove` FlowAtom b) = FlowAtom <$> removeSpotifyItem a b
runExpression flow (expr1 `Remove` expr2)           = Remove <$> runExpression flow expr1 <*> runExpression flow expr2

trackURI (ID tid) = fetchTrackBaseURI <> encodeUtf8 tid

fetchTrackBaseURI     = "https://api.spotify.com/v1/tracks/"
fetchPlaylistBaseURI = "https://api.spotify.com/v1/users/" -- spotify/playlists/"

fetchTracksURI :: Track -> ByteString
fetchTracksURI tracks = fetchTrackBaseURI `appendQueryParam` [("ids", encodeUtf8 idStr)]
    where
      idStr = intercalate "," $ fmap extractID tids
      tids = getTrackIDs tracks
      extractID (ID v) = v

getTrackIDs :: Track -> [ID]
getTrackIDs (Track _ _ id) = [id]
getTrackIDs (TrackNotLoaded id) = [id]
getTrackIDs (TrackList tracks) = foldr (\track ids -> getTrackIDs track <> ids) mempty tracks

fetchPlaylistURI :: Playlist -> ByteString
fetchPlaylistURI (PlaylistNotLoaded (ID id) (User user)) = fetchPlaylistBaseURI <> encodeUtf8 user <> "/playlists/" <> encodeUtf8 id

setExpiration :: UTCTime -> Flow -> Flow
setExpiration time flow = flow {flowExpiration = newTime}
    where
      newTime = fmap (\lifetime -> addUTCTime lifetime time) mLifetime
      mLifetime = fromIntegral <$> (expiresIn $ flowToken flow)

isExpired :: UTCTime -> Flow -> Bool
isExpired time flow =
      case exp of
        Nothing -> False
        Just b -> b
    where
      diff expiration = diffUTCTime expiration time
      exp = fmap (\t -> diff t <= 60) $ flowExpiration flow


parseURI :: Text -> SpotifyItem
parseURI uri =
    case splitElem ':' uri of
      ["spotify", "track", id] -> TrackF $ TrackNotLoaded (ID id)
      ["spotify", "user", user, "playlist", id] -> PlaylistF $ PlaylistNotLoaded (ID id) (User user)
      ["spotify", "artist", id] -> ArtistF (ID id)
      _ -> error $ "Invalid uri: " <> show uri

getSpotifyItem :: Expression -> SpotifyItem
getSpotifyItem (FlowAtom a) = a
getSpotifyItem _ = error "The expression is not simplified!"