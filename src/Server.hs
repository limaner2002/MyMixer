{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Server
  ( runServer
  , Network.Wai.Handler.Warp.Port
  ) where

import ClassyPrelude
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types (status200, QueryItem)
import Network.HTTP.Types.Header (hContentType)

runServer :: (MonadBaseControl IO m, Forall (Pure m)) => Port -> m [QueryItem]
runServer port = do
  mVar <- newEmptyMVar
  res <- race (liftBase $ run port (app mVar))
        (takeMVar mVar)
  case res of
    Left _ -> fail "No code was provided in the request."
    Right qItems -> return qItems
  

app :: MVar [QueryItem] -> Application
app mVar req f = do
  resprcvd <- f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
  putMVar mVar $ queryString req
  return resprcvd
