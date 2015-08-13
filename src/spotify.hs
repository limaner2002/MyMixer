{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import           Network.HTTP.Conduit
import Flow
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Keys

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  res <- (evalStateT . runExceptT)
         (flowGetBS "http://localhost") (
                   OAuth2WebServerFlow
                   { flowToken = Nothing
                   , oauth2 = spotifyKey
                   , manager = mgr
                   , flowScope = spotifyScope
                   }
                  )
  print res


