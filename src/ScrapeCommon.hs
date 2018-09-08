{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScrapeCommon where

import ClassyPrelude
import Servant.Client
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

newtype HostUrl = HostUrl String
  deriving (Show, Eq, IsString)

clientEnv :: HostUrl -> IO ClientEnv
clientEnv (HostUrl url) = do
  mgr <- C.newManager TLS.tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https url 443 "") Nothing

clientEnvDbgWith :: (C.Request -> IO C.Request) -> HostUrl -> IO ClientEnv
clientEnvDbgWith f (HostUrl url) = do
  mgr <- C.newManager settings
  return $ ClientEnv mgr (BaseUrl Https url 443 "") Nothing
    where
      reqLoggerFunc req = f req
      settings = TLS.tlsManagerSettings { C.managerModifyRequest = reqLoggerFunc
                                        , C.managerModifyResponse = respLoggerFunc
                                        }
      respLoggerFunc resp = do
        print (C.responseStatus resp)
        print (C.responseHeaders resp)
        print (C.responseCookieJar resp)
        return resp

clientEnvDbg :: HostUrl -> IO ClientEnv
clientEnvDbg = clientEnvDbgWith (\req -> print req >> return req)
