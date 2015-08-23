{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import           Network.HTTP.Conduit
import Flow
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Keys
import Data.Time

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  curTime <- getCurrentTime
  res <- (evalStateT . runExceptT)
         (getToken) (
                   OAuth2WebServerFlow
                   { flowToken = Nothing
                   , oauth2 = spotifyKey
                   , manager = mgr
                   , flowScope = spotifyScope
                   , timestamp = curTime
                   , authService = "My Spotify Mixer 0.2"
                   , authAccount = "MyMixer"
                   }
                  )
  print res


