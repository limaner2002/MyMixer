{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Arrows            #-}

module WeightedShuffle where

import ClassyPrelude
import WeightedMap
import Data.Random (MonadRandom, Distribution, Uniform)
import qualified Data.Random as Rand
import Control.Arrow
import MachineUtils

sample :: (MonadRandom b, MonadBase b m, Distribution Uniform a, Num a) => Kleisli m a a
sample = Kleisli (liftBase . Rand.sample . Rand.uniform 0)

sampleItem :: (Arrow a, WeightedDict weighted k v, ContainerKey (weighted k v) ~ k, MapValue (weighted k v) ~ v, Ord k) => a (k, weighted k v) (Maybe (k, v))
sampleItem = arr (uncurry lookupWeighted)

weightedSample :: (MonadRandom b, MonadBase b m, Distribution Uniform k, WeightedDict weighted k v, Num k, ContainerKey (weighted k v) ~ k, MapValue (weighted k v) ~ v, Ord k) =>
                  Kleisli m (k, weighted k v) (Maybe (k, v))
weightedSample = proc (maxK, m) -> do
  k <- sample -< maxK
  item <- sampleItem -< (k, m)
  returnA -< item

weightedSampleRemove :: (MonadRandom b, MonadBase b m, Distribution Uniform k, WeightedDict weighted k v, BiPolyMap weighted, ContainerKey (weighted k v) ~ k, MapValue (weighted k v) ~ v, BPMKeyConstraint weighted k,
                         Num k, Ord k) =>
                        Kleisli m (k, weighted k v) (k, weighted k v, Maybe v)
weightedSampleRemove = proc (maxK, m) -> do
  mItem <- weightedSample -< (maxK, m)
  case mItem of
    Nothing -> returnA -< (maxK, m, Nothing)
    Just (k, item) -> do
      newM <- arr (uncurry deleteMap) -< (k, m)
      returnA -< (k, newM, Just item)

weightedSamplesRemove :: (MonadRandom b, MonadBase b m, Distribution Uniform k, WeightedDict weighted k v, ContainerKey (weighted k v) ~ k, MapValue (weighted k v) ~ v, BPMKeyConstraint weighted k, BiPolyMap weighted,
                          Num k, Ord k, Monad m) =>
                         Int ->
                         ProcessA (Kleisli m) (Event (k, weighted k v)) (Event (Maybe (k, v)))
weightedSamplesRemove n = constructT kleisli0 go
  where
    go = do
      (k, m) <- await
      loop k m n
    loop _ _ 0 = go
    loop k m n = do
      case onull m of
        True -> do
          yield Nothing
          go
        False -> do
          (k', m', v) <- lift $ runKleisli weightedSampleRemove (k, m)
          yield $ (,) <$> pure k' <*> v
          loop (k - k') m' (n - 1)

newtype NoItemException = NoItemException Text
  deriving Show

instance Exception NoItemException

asMonadThrow :: MonadThrow m => Maybe a -> m a
asMonadThrow Nothing = throwM $ NoItemException "There was no item present!"
asMonadThrow (Just v) = pure v

-- simpleCache :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event k) (Event v) -> ProcessA a (Event v) (Event (Maybe v)) -> ProcessA a (Event k, Event (map k v)) (Event v, Event (map k v))
-- simpleCache act update = proc (kEvt, cache) -> do
--   mVal <- mergeEvents >>> evMap (uncurry lookup) >>> hold Nothing -< (kEvt, cache)
--   case mVal of
--     Nothing -> do
--       val <- act -< kEvt
--       ev <- mergeEvents -< (kEvt, val)
--       cache' <- mergeEvents >>> evMap (\(m, (k', v)) -> insertMap k' v m) -< (cache, ev)
--       returnA -< (val, cache')
--   --   Just val -> do
--   --     ev <- mergeEvents -< (kEvt, val)
--   --     cache' <- mergeEvents >>> evMap (\x -> _) -< (ev, cache)
--   --     returnA -< (val, cache')

-- simpleCache :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event (Maybe v)) (Event (Maybe v)) -> ProcessA a (Event k) (Event v)
-- simpleCache act = loop (simpleCache_ act)

simpleCache :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event (Either k v)) (Event (Maybe v)) -> ProcessA a (Event k, Event (map k v)) (Event v, Event (map k v))
simpleCache act = proc (kEvt, cache) -> do
  cache' <- mergeEvents >>> alterMapWithKeyP act -< (kEvt, cache)
  cache'' <- evMap snd -< cache'
  v <- evMap (uncurry lookup) >>? id -< cache'
  returnA -< (v, cache'')

alterMapA :: (ArrowChoice a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => a (Maybe v) (Maybe v) -> k -> a (map k v) (map k v)
alterMapA f k = proc map -> do
  mVal <- arr (lookup k) >>> f -< map
  case mVal of
    Nothing -> arr (deleteMap k) >>> returnA -< map
    Just v -> arr (uncurry $ insertMap k) >>> returnA -< (v, map)

updateMapP :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event v) (Event (Maybe v)) -> ProcessA a (Event (k, map k v)) (Event (map k v))
updateMapP f = proc map -> do
  mVal <- evMap (uncurry lookup) >>> hold Nothing -< map
  case mVal of
    Nothing -> evMap snd >>> returnA -< map
    Just v -> do
      mV' <- f >>> hold Nothing -< (v <$ map)
      case mV' of
        Nothing -> evMap (uncurry deleteMap) >>> returnA -< map
                   -- This could use some cleaning up
        Just v' -> evMap (\(v, (k, m)) -> insertMap k v m) >>> returnA -< fmap (\m -> (v, m)) map

alterMapP :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event (Maybe v)) (Event (Maybe v)) -> ProcessA a (Event (k, map k v)) (Event (k, map k v))
alterMapP f = proc map -> do
  mVal <- evMap (uncurry lookup) >>> f >>> hold Nothing -< map
  case mVal of
    Nothing -> evMap (\(k, m) -> (k, deleteMap k m)) >>> returnA -< map
    Just v -> evMap (\(v, (k, m)) -> (k, insertMap k v m)) >>> returnA -< fmap (\m -> (v, m)) map

alterMapWithKeyP :: (ArrowApply a, IsMap (map k v), ContainerKey (map k v) ~ k, MapValue (map k v) ~ v) => ProcessA a (Event (Either k v)) (Event (Maybe v)) -> ProcessA a (Event (k, map k v)) (Event (k, map k v))
alterMapWithKeyP f = proc map -> do
  mVal <- evMap (\(k, m) -> (k, lookup k m)) >>> evMap toEither >>> f >>> hold Nothing -< map
  case mVal of
    Nothing -> evMap (\(k, m) -> (k, deleteMap k m)) >>> returnA -< map
    Just v -> evMap (\(v, (k, m)) -> (k, insertMap k v m)) >>> returnA -< fmap (\m -> (v, m)) map
  where
    toEither (k, Nothing) = Left k
    toEither (k, Just v) = Right v

(>>?) :: ArrowApply cat => ProcessA cat (Event a) (Event (Maybe b)) -> ProcessA cat (Event b) (Event c) -> ProcessA cat (Event a) (Event c)
(>>?) catA catB = proc input -> do
  mV <- catA >>> hold Nothing -< input
  case mV of
    Nothing -> returnA -< noEvent
    Just v -> catB >>> returnA -< v <$ input

infixr 1 >>?

(>>|) :: ArrowApply cat => ProcessA cat (Event a) (Event d) -> ProcessA cat (Event b) (Event d) -> ProcessA cat (Event (Either a b)) (Event d)
(>>|) catA catB = proc input -> do
  mV <- evMap Just >>> hold Nothing -< input
  case mV of
    Nothing -> returnA -< noEvent
    Just (Left v) -> catA >>> returnA -< v <$ input
    Just (Right v) -> catB >>> returnA -< v <$ input

infixr 5 >>|
