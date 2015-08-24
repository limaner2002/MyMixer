{-# LANGUAGE OverloadedStrings #-}
module Flow where
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Time
import System.IO (stderr)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist (insert)

import Model
import Keychain

type Flow = SqlPersistT (NoLoggingT (ResourceT (ExceptT BL.ByteString (StateT OAuth2WebServerFlow IO))))

throwError = lift . lift . lift . throwE

flowGetJSON :: FromJSON a => URI -> Flow (OAuth2Result a)
flowGetJSON uri = do
  tok <- checkToken
  flow <- get
  let mgr = manager flow
  liftIO $ authGetJSON mgr tok uri

flowGetBS :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS uri = do
  tok <- checkToken
  flow <- get
  let mgr = manager flow

  liftIO $ authGetBS mgr tok uri

flowGetBS' :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS' uri = do
  tok <- checkToken
  flow <- get
  let mgr = manager flow

  liftIO $ authGetBS' mgr tok uri

flowPostJSON :: URI -> PostBody -> Flow (OAuth2Result BL.ByteString)
flowPostJSON uri pb = do
  tok <- checkToken
  flow <- get
  let mgr = manager flow
  liftIO $ authPostBS mgr tok uri pb

-- | Checks to see if the flow has a token and fetches a new one if
-- one does not exist or refreshes an expired one
--
--
checkToken :: Flow AccessToken
checkToken = do
  flow <- get
  case flowToken flow of
             Nothing -> do
               liftIO $ BS.hPutStrLn stderr "No valid token found. Requesting new one"
               getToken
               flow <- get
               case flowToken flow of
                 Nothing -> throwError $ BL.concat ["Invalid token in function 'checkToken'"]
                 Just tok -> return tok
             Just tok -> do
                        now <- liftIO getCurrentTime
                        if (diffUTCTime now (timestamp flow)) > 3300
                        then get >>= (\x -> extractMaybe flowToken x "checkToken: Token should have refreshed, but it is not here for some reason.")
                        else return tok

getToken :: Flow ()
getToken = do
  flow <- get
  let mgr = manager flow
      key = oauth2 flow
      scope = flowScope flow
  liftIO $ BS.putStrLn $ authorizationUrl key `appendQueryParam` scope
  liftIO $ putStrLn "visit the url and paste the code here: "
  code <- liftIO $ fmap BS.pack getLine
  res <- liftIO $ fetchAccessToken mgr key code
  tok <- handleResult res "Could not get token: "
  liftIO $ saveRefreshToken (authService flow) (authAccount flow) (refreshToken tok)
  insert $ fromFlow flow
  curTime <- liftIO getCurrentTime
  put $ flow { flowToken = Just $ tok
             , timestamp = curTime
             }

refreshAuthToken :: Flow ()
refreshAuthToken = do
  flow <- get
  let mgr = manager flow
      key = oauth2 flow

  flowToken <- extractMaybe flowToken flow "Could not refresh token. No token found."
  rTok <- extractMaybe refreshToken flowToken "Could not refresh token. No refresh token found."

  res <- liftIO $ fetchRefreshToken mgr key rTok
  tok <- handleResult res "Could not refresh token: "
  curTime <- liftIO getCurrentTime
  insert $ fromFlow flow
  put $ flow { flowToken = Just tok
             , timestamp = curTime
             }

handleResult :: OAuth2Result a -> BL.ByteString -> Flow a
handleResult (Left hoauthMsg) msg = throwError $ BL.concat [msg, hoauthMsg]
handleResult (Right val) _ = return val

extractMaybe :: (a -> Maybe b) -> a -> BL.ByteString -> Flow b
extractMaybe f x msg = do
  case f x of
    Nothing -> throwError msg
    Just val -> return val