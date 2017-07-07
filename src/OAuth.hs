{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module OAuth where

import ClassyPrelude
import Data.ByteString.Streaming.HTTP
import qualified Data.ByteString.Streaming as BSS
import Network.HTTP.Types.Header
import Control.Arrow
import Data.Aeson
import qualified Streaming.Prelude as S
import Data.Attoparsec.ByteString.Streaming
import Control.Lens
import Data.Time
import Data.Time.Lens

data AccessToken = AccessToken
  { _token :: ByteString
  , _tokenType :: TokenType
  , _expiresIn :: Int
  } deriving Show

instance FromJSON AccessToken where
  parseJSON (Object o) = AccessToken
    <$> (encodeUtf8 <$> o .: "access_token")
    <*> o .: "token_type"
    <*> o .: "expires_in"
  parseJSON _ = fail "could not decode Access Token"

data TokenType
  = Bearer
  deriving Show

instance FromJSON TokenType where
  parseJSON (String "bearer") = pure Bearer
  parseJSON (String "Bearer") = pure Bearer
  parseJSON _ = fail "unrecognized token type"

makeLenses ''AccessToken

addRequestHeader headerName val req = req { requestHeaders = (headerName, val) : requestHeaders req }

getClientToken :: MonadResource m => Manager -> String -> ByteString -> ByteString -> m (Either String AccessToken)
getClientToken mgr url clientId clientSecret = do
  req <- clientTokenReq url clientId clientSecret
  body <- getClientToken_ mgr req
  return $ eitherDecode body

getClientToken_ mgr req = do
  resp <- http req mgr
  body <- S.fst' <$> (responseBody >>> BSS.toLazy $ resp)
  return body

clientTokenReq url clientId clientSecret = urlEncodedBody
    [ ("grant_type", "client_credentials")
    , ("client_id", clientId)
    , ("client_secret", clientSecret)
    ] <$> parseRequest url

data TokenState a = TokenState
  { _tokenStateToken :: AccessToken
  , _expiresAt :: UTCTime
  , _request :: Maybe a
  } deriving Show

makeLenses ''TokenState

tokenMgr :: MonadResource m => Manager -> String -> ByteString -> ByteString -> S.Stream (S.Of a) m r -> S.Stream (S.Of (Either String (TokenState a))) m r
tokenMgr mgr url clientId clientSecret stream = S.scanM checkToken getToken pure stream
  where
    getToken = do
      putStrLn "Getting new token"
      eTok <- getClientToken mgr url clientId clientSecret
      case eTok of
        Left msg -> return $ Left msg
        Right tok -> do
          ct <- liftBase getCurrentTime
          return $ Right (TokenState tok (modL seconds (+ (fromIntegral $ tok ^. expiresIn)) ct) Nothing)
    checkToken (Left msg) _ = return $ Left msg
    checkToken (Right tokenState) a = do
      ct <- liftBase getCurrentTime
            -- Check if the token will expire in the next minute.
      case diffUTCTime (tokenState ^. expiresAt) ct < fromIntegral 60 of
        True -> do
          putStrLn "The token has expired. Getting a new one"
          tok <- getToken
          return $ tok & _Right %~ (request .~ (Just a))
        False -> return $ Right $ tokenState & request .~ (Just a)

reqMgr :: Monad m =>
     S.Stream (S.Of (Either t (TokenState Request))) m r
     -> S.Stream
          (S.Of Request)
          m r
reqMgr stream = S.concat >>> S.map addToReq >>> S.concat $ stream
  where
    addToReq tokenState = req & _Just %~ addRequestHeader "Authorization" ("bearer " <> tok ^. token)
      where
        tok = tokenState ^. tokenStateToken
        req = tokenState ^. request

delay :: MonadBase IO m => (a -> Int) -> S.Stream (S.Of a) m r -> S.Stream (S.Of a) m r
delay f stream = loop stream
  where
    loop strm = do
      eStep <- lift $ S.next strm
      case eStep of
        Left r -> return r
        Right (a, resume) -> do
          S.yield a
          threadDelay (f a)
          loop resume

isLeft (Left _) = True
isLeft _ = False

           -- Everything below should be in a separate module

mcId :: ByteString
mcId = "MCMediaApp"

mcSecret :: ByteString
mcSecret = "Pn|s#Y1CT^N{5z_{upWD"

mcTokUrl :: String
mcTokUrl = "http://nowplayingservices.musicchoice.com/api/token"

addId :: Int -> Request -> Request
addId stationId req = req { path = path req <> encodeUtf8 (tshow stationId) }

spotifySecret :: ByteString
spotifySecret = "216f57bcb8fa4d14971f1ef18ab8fa1c"

spotifyId :: ByteString
spotifyId = "f643c7bbc0d14d348ef188308674b192"

spotifyTokUrl :: String
spotifyTokUrl = "https://accounts.spotify.com/api/token"
