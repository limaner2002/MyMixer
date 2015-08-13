{-# LANGUAGE OverloadedStrings #-}
module Flow where
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Aeson
import System.IO (stderr)

import Keychain

data OAuth2WebServerFlow = OAuth2WebServerFlow
    { flowToken :: Maybe AccessToken
    , oauth2 :: OAuth2
    , manager :: Manager
    , flowScope :: [(BS.ByteString, BS.ByteString)]
    }

instance Show OAuth2WebServerFlow where
    show flow = show token ++ "\n" ++ show oth2
        where
          token = flowToken flow
          oth2 = oauth2 flow

type Flow = ExceptT BL.ByteString (StateT OAuth2WebServerFlow IO)

flowGetJSON :: FromJSON a => URI -> Flow (OAuth2Result a)
flowGetJSON uri = do
  flow <- get
  tok <- checkToken flow
  let mgr = manager flow
  liftIO $ authGetJSON mgr tok uri

flowGetBS :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS uri = do
  flow <- get
  tok <- checkToken flow
  let mgr = manager flow
  liftIO $ authGetBS mgr tok uri

flowGetBS' :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS' uri = do
  flow <- get
  tok <- checkToken flow
  let mgr = manager flow
  liftIO $ authGetBS' mgr tok uri

flowPostJSON :: URI -> PostBody -> Flow (OAuth2Result BL.ByteString)
flowPostJSON uri pb = do
  flow <- get
  tok <- checkToken flow
  let mgr = manager flow
  liftIO $ authPostBS mgr tok uri pb

-- | Checks to see if the flow has a token and fetches a new one if
-- one does not exist or refreshes an expired one
--
--
checkToken :: OAuth2WebServerFlow -> Flow AccessToken
checkToken flow = case mTok of
             Nothing -> do
               liftIO $ BS.hPutStrLn stderr "No valid token found. Requesting new one"
               getToken
             -- TODO: Check to see if the token has expired
             Just tok -> return tok
    where
      mTok = flowToken flow

getToken :: Flow AccessToken
getToken = do
  flow <- get
  let mgr = manager flow
      key = oauth2 flow
      scope = flowScope flow
  liftIO $ BS.putStrLn $ authorizationUrl key `appendQueryParam` scope
  liftIO $ putStrLn "visit the url and paste the code here: "
  code <- liftIO $ fmap BS.pack getLine
  res <- liftIO $ fetchAccessToken mgr key code
  case res of
    Left msg -> throwE msg
    Right tok -> return tok

