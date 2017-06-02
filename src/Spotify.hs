{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Spotify where

import ClassyPrelude
import MachineUtils
import Network.HTTP.Base (urlEncode)
import qualified Web.Browser as WB
import Network.HTTP.Simple
import Network.HTTP.Conduit
import qualified Data.ByteString.Base64 as B64
import Network.HTTP.Types (HeaderName)
import Data.Aeson
import Control.Monad.Trans.Resource
import Server

data SpotifyScope
  = PlaylistReadPrivate
  | PlaylistReadCollaborative
  | PlaylistModifyPublic
  | PlaylistModifyPrivate
  | Streaming
  | UserFollowModify
  | UserFollowRead
  | UserLibraryRead
  | UserLibraryModify
  | UserReadPrivate
  | UserReadBirthdate
  | UserReadEmail
  | UserTopRead
  deriving (Show, Eq, Ord)

dispSpotifyScope :: SpotifyScope -> String
dispSpotifyScope PlaylistReadPrivate = "playlist-read-private"
dispSpotifyScope PlaylistReadCollaborative = "playlist-read-collaborative"
dispSpotifyScope PlaylistModifyPublic = "playlist-modify-public"
dispSpotifyScope PlaylistModifyPrivate = "playlist-modify-private"
dispSpotifyScope Streaming = "streaming"
dispSpotifyScope UserFollowModify = "user-follow-modify"
dispSpotifyScope UserFollowRead = "user-follow-read"
dispSpotifyScope UserLibraryRead = "user-library-read"
dispSpotifyScope UserLibraryModify = "user-library-modify"
dispSpotifyScope UserReadPrivate = "user-read-private"
dispSpotifyScope UserReadBirthdate = "user-read-birthdate"
dispSpotifyScope UserReadEmail = "user-read-email"
dispSpotifyScope UserTopRead = "user-top-read"


instance FromJSON SpotifyScope where
  parseJSON (String "playlist-read-private") = pure PlaylistReadPrivate
  parseJSON (String "playlist-read-collaborative") = pure PlaylistReadCollaborative
  parseJSON (String "playlist-modify-public") = pure PlaylistModifyPublic
  parseJSON (String "playlist-modify-private") = pure PlaylistModifyPrivate
  parseJSON (String "streaming") = pure Streaming
  parseJSON (String "user-follow-modify") = pure UserFollowModify
  parseJSON (String "user-follow-read") = pure UserFollowRead
  parseJSON (String "user-library-read") = pure UserLibraryRead
  parseJSON (String "user-library-modify") = pure UserLibraryModify
  parseJSON (String "user-read-private") = pure UserReadPrivate
  parseJSON (String "user-read-birthdate") = pure UserReadBirthdate
  parseJSON (String "user-read-email") = pure UserReadEmail
  parseJSON (String "user-top-read") = pure UserTopRead
  parseJSON v = decodeFail v "SpotifyScope"

newtype ClientID = ClientID String

newtype URL a = URL String
  deriving Show

data Base = Base
  deriving Show

data Redirect = Redirect
  deriving Show

data Auth = Auth
  deriving Show

data Token = Token
  deriving Show

data ResponseType = RespCode
  deriving Show

newtype AuthCode = AuthCode ByteString
  deriving (Show, Eq, Read)

dispResponseType :: ResponseType -> String
dispResponseType RespCode = "code"

fromURL :: URL a -> String
fromURL (URL u) = u

encodeURL :: URL a -> String
encodeURL (URL u) = encodeIt u

encodeIt :: String -> String
encodeIt = urlEncode

authorizationURL :: URL Base -> ClientID -> ResponseType -> URL Redirect -> [SpotifyScope] -> URL Auth
authorizationURL base (ClientID cid) respType redir scopes
  = URL (fromURL base <> "/?client_id=" <> cid <> "&response_type=" <> dispResponseType respType <> "&redirect_uri=" <> encodeURL redir <> dispScopes)
  where
    dispScopes = "&scope=" <> (encodeIt $ intercalate " " $ fmap dispSpotifyScope scopes)
    
testAuthorization :: URL Auth
testAuthorization = authorizationURL base cid RespCode testRedirect scopes
  where
    scopes = [UserReadPrivate, UserReadEmail]
    base = URL "https://accounts.spotify.com/authorize" :: URL Base

cis :: ByteString
cis = "216f57bcb8fa4d14971f1ef18ab8fa1c"

cid :: ClientID
cid = ClientID "f643c7bbc0d14d348ef188308674b192"

fromClientID :: ClientID -> String
fromClientID (ClientID cid) = cid

tokenURL :: URL Token
tokenURL = URL "https://accounts.spotify.com/api/token"

tokenReq :: Arrow a => a Request Request
tokenReq = arr (setRequestBody tokenReqBody) >>> arr (setRequestMethod "POST") >>> arr (setRequestHeaders tokenReqHeader)

tokenReqBody :: RequestBody
tokenReqBody = RequestBodyBS body
  where
    body = intercalate "&" [ "grant_type=authorization_code"
           , "code=" <> code
           , "redirect_uri=" <> (encodeUtf8 . pack . encodeURL $ testRedirect)
           -- , "Authorization=basic"
           -- , "client_id=" <> (encodeUtf8 . pack . fromClientID $ cid)
           -- , "client_secret=" <> cis
           ]

tokenReqHeader :: [(HeaderName, ByteString)]
tokenReqHeader = [ ("Authorization", "Basic " <> B64.encode ((encodeUtf8 . pack . fromClientID) cid <> ":" <> cis))
                 , ("Content-Type", "application/x-www-form-urlencoded")
                 ]

code :: ByteString
code = "AQCgwERV-daUPFBddmkkbPWunSdIfyyzK5Qjza-nbob37DZzadnkVh3kpnngU1q7z8xaK5fPy69paLYKaFO6SedFzB4z8s34p2rHZMFJKpMCjdAJNw4wcII5ibWgaXzercZoWpZP-lrMAYgji-8k-sgOrk_SXzCvjzcpMX-NcuFXemUReJKNu6eE7Qpr8avDIw6Jk4f6jfs6vTHQVH5LSedsUcowfqnDJs2mYkpyng"

testRedirect :: URL Redirect
testRedirect = URL "http://localhost:4242"

data SpotifyAccessToken = SpotifyAccessToken
  { token :: AccessToken
  , tokenType :: TokenType
  , tokenScope :: [SpotifyScope]
  , tokenExpiration :: Int
  , tokenRefresh :: RefreshToken
  } deriving Show

instance FromJSON SpotifyAccessToken where
  parseJSON (Object o) =
    SpotifyAccessToken <$> o .: "access_token"
                <*> o .: "token_type"
                <*> parseScopes
                <*> o .: "expires_in"
                <*> o .: "refresh_token"
    where
      parseScopes = do
        scope <- o .: "scope"
        let scopeList = sequence $ fmap fromJSON $ fmap String $ words scope
        case scopeList of
          Success scopes -> return scopes
          Error msg -> fail msg
  parseJSON v = decodeFail v "SpotifyAccessToken"

newtype RefreshToken = RefreshToken ByteString
  deriving Show

instance FromJSON RefreshToken where
  parseJSON (String s) =
    pure $ RefreshToken $ encodeUtf8 s
  parseJSON v = decodeFail v "RefreshToken"

newtype AccessToken = AccessToken ByteString
  deriving Show

instance FromJSON AccessToken where
  parseJSON (String s) =
    pure $ AccessToken $ encodeUtf8 s
  parseJSON v = decodeFail v "AccessToken"

data TokenType = Bearer
  deriving Show

instance FromJSON TokenType where
  parseJSON (String "Bearer") = pure Bearer
  parseJSON v = decodeFail v "TokenType"

decodeFail :: Monad m => Value -> String -> m a
decodeFail v typ = fail $ dispVal <> " does not appear to be of type '" <> typ <> "'"
  where
    dispVal = 
      case rem == mempty of
        True -> val
        False -> val <> "..."
    (val, rem) = arr show >>> take n &&& drop n $ v
    n = 75

openBrowser :: MonadIO m => ProcessA (Kleisli m) (Event (URL a)) (Event Bool)
openBrowser = machine openBrowser_

openBrowser_ :: MonadIO m => URL a -> m Bool
openBrowser_ = liftIO . WB.openBrowser . fromURL

getToken :: MonadResource m =>
            ProcessA (Kleisli m) (Event (URL a, Manager)) (Event ByteString)
getToken =
      evMap fst &&& evMap snd
  >>> (    (machine (parseUrlThrow . fromURL) >>> anytime tokenReq)
       *** id
      )
  >>> mergeEvents
  >>> makeRequest
  >>> sourceHttp_
  >>> evMap snd

getAuthCode :: (MonadIO m, MonadBaseControl IO m, Forall (Pure m)) =>
               ProcessA
               (Kleisli m)
               (Event (URL a, Port))
               (Event (Maybe AuthCode))
getAuthCode = evMap fst &&& evMap snd >>> openBrowser *** machine runServer >>> arr snd >>> evMap (queryMap . mapFromList) >>> evMap (join . lookup "code") >>> evMap (fmap AuthCode)

queryMap :: Map ByteString (Maybe ByteString) -> Map ByteString (Maybe ByteString)
queryMap = id
