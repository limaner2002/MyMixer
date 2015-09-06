{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards   #-}

-- module Model ( SourcePlaylists
--              , fromToken
--              , fromFlow
--              , toToken
--              , toFlow
--              , OAuth2WebServerFlow (..)
--              , AccessTokenEntry
--              , FlowEntry
--              ) where
module Model where
import Database.Persist.TH
import Database.Persist.Sql (SqlPersistT)
import Database.Persist (insert)
import Network.OAuth.OAuth2
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Conduit (Manager)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AccessTokenEntry
  accessToken BS.ByteString
  expiresIn Int Maybe
  tokenType BS.ByteString Maybe
FlowEntry
  token AccessTokenEntryId Maybe
  timestamp UTCTime
  deriving Show
SourcePlaylists
  uuid T.Text
  Primary uuid
SimplifiedPlaylistEntry
  name T.Text
  uri T.Text
  href T.Text
  
|]

fromToken :: AccessToken -> AccessTokenEntry
fromToken AccessToken {..} = AccessTokenEntry {
                               accessTokenEntryAccessToken = accessToken
                             , accessTokenEntryExpiresIn = expiresIn
                             , accessTokenEntryTokenType = tokenType
                             }

fromFlow :: Maybe AccessTokenEntryId -> OAuth2WebServerFlow -> FlowEntry
fromFlow tokId OAuth2WebServerFlow {..} = FlowEntry
                           { flowEntryToken = tokId
                           , flowEntryTimestamp = timestamp
                           }

toToken :: Maybe BS.ByteString -> AccessTokenEntry -> AccessToken
toToken refreshToken AccessTokenEntry {..} =
    AccessToken
    { accessToken = accessTokenEntryAccessToken
    , expiresIn = accessTokenEntryExpiresIn
    , tokenType = accessTokenEntryTokenType
    , refreshToken = refreshToken
    }

toFlow :: Maybe AccessTokenEntry -> Maybe BS.ByteString -> FlowEntry -> OAuth2WebServerFlow
toFlow tokEntry refreshToken flowEntry =
    OAuth2WebServerFlow
    { flowToken = fmap (toToken refreshToken) tokEntry
    , timestamp = tStamp
    }
    where
      tStamp = flowEntryTimestamp flowEntry

-- insertFlow :: OAuth2WebServerFlow ->
-- insertFlow flow = do
--   case flowToken flow of
--     Nothing -> 

data OAuth2WebServerFlow = OAuth2WebServerFlow
    { flowToken :: Maybe AccessToken                -- The token used for authorized requests
    , oauth2 :: OAuth2                              -- The app key
    , manager :: Manager                            -- Connection manager
    , flowScope :: [(BS.ByteString, BS.ByteString)] -- Scopes to request from service
    , timestamp :: UTCTime                          -- Time when token was fetched. When (now() - timestamp) >= expiresIn, refresh token
    , authService :: String                         -- Service name for keychain entry
    , authAccount :: String                         -- Account name for keychain entry
    }

instance Show OAuth2WebServerFlow where
    show flow = show token ++ "\n" ++ show oth2
        where
          token = flowToken flow
          oth2 = oauth2 flow
