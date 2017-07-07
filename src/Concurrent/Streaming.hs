{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Concurrent.Streaming where

import ClassyPrelude
import qualified Streaming.Prelude as S
import qualified Streaming.Internal as SI
import Streaming hiding ((<>))
import Control.Arrow
import OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Resource
import Control.Lens hiding ((:>))

chanIn :: MonadIO m => Int -> TChan (Maybe b) -> S.Stream (S.Of b) m ()
chanIn n chan = go n
  where
    go 0 = return ()
    go n = do
      mVal <- atomically $ readTChan chan
      case mVal of
        Nothing -> go (n - 1)
        Just v -> do
          S.yield v
          go n

chanOut :: MonadIO m => TChan (Maybe b) -> S.Stream (S.Of b) m r -> m r
chanOut chan stream = loop stream
  where
    loop stream' = do
      eVal <- S.next stream'
      case eVal of
        Left r -> do
          atomically . writeTChan chan $ Nothing
          return r
        Right (val, resume) -> do
          atomically . writeTChan chan . Just $ val
          loop resume

concurrentMerge :: (MonadIO m, MonadBaseControl IO m, Forall (Pure m)) => [S.Stream (S.Of a) m r] -> S.Stream (S.Of a) m ()
concurrentMerge streams = do
  chan <- lift . liftBase $ newTChanIO
  lift . fork . void $ mapConcurrently (chanOut chan) streams
  chanIn (length streams) chan

syncChanIn :: MonadBase IO m => TChan (Either r b) -> TChan (TChan (Either r b)) -> S.Stream (S.Of b) m r --m (Either r (b, S.Stream (S.Of b) m r))
syncChanIn chanIn chanReq = loop
  where
    loop = do
      liftBase . atomically . writeTChan chanReq $ chanIn
      mVal <- liftBase . atomically . readTChan $ chanIn
      case mVal of
        Left r -> SI.Return r
        Right val -> SI.Step (val :> loop)

syncChanOut :: MonadBase IO m => TChan (TChan (Either r b)) -> S.Stream (S.Of b) m r -> [TChan (Either r b)] -> m r
syncChanOut chanReq stream chans = loop stream
  where
    loop stream' = do
      chan <- liftBase . atomically . readTChan $ chanReq
      eVal <- S.next stream'
      case eVal of
        Left r -> do
          mapM_ (\c -> liftBase . atomically . writeTChan c . Left $ r) chans
          return r
        Right (val, resume) -> do
          liftBase . atomically . writeTChan chan . Right $ val
          loop resume

concurrentFan :: (MonadBaseControl IO m, Forall (Pure m)) => S.Stream (S.Of a) m r -> [TChan (Either r a)] -> m [S.Stream (S.Of a) m r]
concurrentFan inStream chans = do
  chanReq <- liftBase $ newTChanIO
  fork . void $ syncChanOut chanReq inStream chans
  mapConcurrently (\chan -> return $ syncChanIn chan chanReq) chans

concurrentFan' :: (MonadBaseControl IO m, Forall (Pure m)) => S.Stream (S.Of a) m r -> [S.Stream (S.Of a) m r -> S.Stream (S.Of b) m r] -> m [S.Stream (S.Of b) m r]
concurrentFan' inStream streamFuncs = do
  chans <- liftBase $ sequence $ take (length streamFuncs) $ repeat newTChanIO
  fs <- concurrentFan inStream chans
  let fs' = fmap (\(f, f') -> f f') $ zip streamFuncs fs
  return fs'

    -- Everything below should be in a separate module

-- main :: IO ()
-- main = do
--   let src = S.each [1..20 :: Int]
--   fs <- concurrentFan' src [ S.map (\x -> "f1 took: " <> show x) >>> S.chain (const $ threadDelay 1000000)
--                            , S.take 3 >>> S.map (\x -> "f2 took: " <> show x) >>> S.chain (const $ threadDelay 2000000)
--                            ]
--   concurrentMerge >>> S.print $ fs

-- scrapeStations :: (MonadResource m, Forall (Pure m), MonadBaseControl IO m) => m ()
-- scrapeStations = do
--   req <- parseRequest $ "http://nowplayingservices.musicchoice.com/api/NowPlaying/ttla/"
--   mgr <- liftBase $ newManager tlsManagerSettings
--   let fs = [ getTrack' mgr 36
--            , getTrack' mgr 44
--            ]
--   fs' <- concurrentFan' (reqSrc mgr req) fs
--   concurrentMerge >>> S.print $ fs'

-- reqSrc :: MonadResource m =>
--      Manager
--      -> Request
--      -> Stream
--           (Of Request) m ()
-- reqSrc mgr req = S.each >>> tokenMgr mgr mcTokUrl mcId mcSecret >>> reqMgr $ repeat req

-- getTrack' :: MonadResource m => Manager -> Int -> S.Stream (S.Of Request) m r -> S.Stream (S.Of MCTrack) m r
-- getTrack' mgr stationId = S.map (addId stationId)
--   >>> getTrack mgr
--   >>> delay (\t -> t ^. ttl * 1000)
--   >>> S.scan holdOld (Nothing, False) id
--   >>> S.filter (\(_, b) -> b)
--   >>> S.map fst
--   >>> S.concat
--   where
--     holdOld (Nothing, _) new = (Just new, True)
--     holdOld (Just old, _) new
--       | old == new = (Just new, False)
--       | otherwise = (Just new, True)
