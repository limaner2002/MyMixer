{-# LANGUAGE OverloadedStrings #-}
module Flow where
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.OAuth.OAuth2 hiding (URI)
import Network.HTTP.Conduit
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Time
import System.IO (stderr)
import Control.Monad.Reader (ReaderT, MonadReader)
import Database.Persist.Sql (SqlPersistT, SqlBackend)
import Control.Monad.Logger (NoLoggingT, LoggingT, logInfoN)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as T
import Database.Persist ( insert
                        , insert_
                        , repsert
                        , selectFirst
                        , entityVal
                        , Entity
                        )

import Model
import Keychain hiding (checkResult)

type Flow = SqlPersistT (NoLoggingT (ResourceT (LoggingT (ExceptT BL.ByteString (StateT OAuth2WebServerFlow IO)))))

type URI = T.Text

repack = C8.pack . T.unpack

throwError = lift . lift . lift . lift . throwE
logInfo = lift . lift . logInfoN

flowGetJSON :: FromJSON a => URI -> Flow (OAuth2Result a)
flowGetJSON uri = do
  tok <- checkToken
  mgr <- gets manager

  liftIO $ authGetJSON mgr tok (repack uri)

flowGetBS :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS uri = do
  tok <- checkToken
  mgr <- gets manager

  liftIO $ authGetBS mgr tok (repack uri)

flowGetBS' :: URI -> Flow (OAuth2Result BL.ByteString)
flowGetBS' uri = do
  tok <- checkToken
  mgr <- gets manager

  liftIO $ authGetBS' mgr tok $ repack uri

flowPostJSON :: URI -> PostBody -> Flow (OAuth2Result BL.ByteString)
flowPostJSON uri pb = do
  tok <- checkToken
  mgr <- gets manager

  liftIO $ authPostBS mgr tok (repack uri) pb

-- | Checks to see if the flow has a token and fetches a new one if
-- one does not exist or refreshes an expired one
--
--
checkToken :: Flow AccessToken -- OAuth2WebServerFlow -> OAuth2WebServerFlow
checkToken = do
  flow <- get
  case flowToken flow of
             Nothing -> do
               logInfo "No valid token found. Requesting new one"
               getToken
               flow <- get
               -- insert $ fromFlow flow
               insertFlow
               extractMaybe flowToken flow "Invalid token in function 'checkToken'"
             Just tok -> do
                        now <- liftIO getCurrentTime
                        if (diffUTCTime now (timestamp flow)) > 3540
                        then do
                          logInfo "Token has expired, refreshing now"
                          refreshAuthToken
                          flow <- get
                          -- insert $ fromFlow flow
                          insertFlow
                          extractMaybe flowToken flow "checkToken: Token should have refreshed, but it is not here for some reason."
                        else return tok

getToken :: Flow ()
getToken = do
  flow <- get
  let mgr = manager flow
      key = oauth2 flow
      scope = flowScope flow
  liftIO $ C8.putStrLn $ authorizationUrl key `appendQueryParam` scope
  liftIO $ putStrLn "visit the url and paste the code here: "
  code <- liftIO $ fmap C8.pack getLine
  res <- liftIO $ fetchAccessToken mgr key code
  tok <- handleResult res "Could not get token: "
  liftIO $ saveRefreshToken (authService flow) (authAccount flow) (refreshToken tok)
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

insertFlow :: Flow ()
insertFlow = do
    flow <- get
    case flowToken flow of
      Nothing -> insert_ $ fromFlow Nothing flow
      Just tok -> do
               let tokId = (AccessTokenEntryKey 1)
               repsert tokId $ fromToken tok
               repsert (FlowEntryKey 1) $ fromFlow (Just tokId) flow

retrieveFlow :: MonadIO m => String -> String -> ReaderT SqlBackend m (Maybe OAuth2WebServerFlow)
retrieveFlow service account = do
  flowEntity <- selectFirst [] []
  tokenEntity <- selectFirst [] []
  refreshToken <- liftIO $ fromKeychain service account
  let flowEntry = fmap entityVal flowEntity
      tokenEntry = fmap entityVal tokenEntity
      rTok = fmap C8.pack refreshToken
      mFlow = fmap (toFlow tokenEntry rTok) (flowEntry)

  return $ mFlow

fetchObject :: FromJSON a => (URI -> Flow a)
fetchObject = (\x -> flowGetJSON x >>= checkResult)

checkResult :: Either BL.ByteString a -> Flow a
checkResult (Left msg) = throwError msg
checkResult (Right val) = return val
