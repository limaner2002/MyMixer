{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module WeightedMap where

import ClassyPrelude

decrement :: (Ord a, Num a) => a -> a -> a
decrement x y
  | x < y = y - x
  | otherwise = y

data WeightMap k v = Node k v (WeightMap k v) (WeightMap k v) | Nil
  deriving (Show, Functor, Foldable, Traversable)

type instance Element (WeightMap k v) = v

instance MonoFunctor (WeightMap k v)

instance MonoFoldable (WeightMap k v)

instance MonoTraversable (WeightMap k v)

instance Ord k => Monoid (WeightMap k v) where
  mempty = Nil

  Nil `mappend` t = t
  t `mappend` Nil = t
  t `mappend` (Node k v left right) =
    new `mappend` left `mappend` right
    where
      new = insert k v t

instance Ord k => Semigroup (WeightMap k v)

instance Ord k => GrowingAppend (WeightMap k v)

instance Ord k => SetContainer (WeightMap k v) where
  type ContainerKey (WeightMap k v) = k

  member k wm = case leastLargest k wm of
                  Nothing -> False
                  Just _ -> True
  notMember k wm = not $ member k wm

  union t1 t2 = t1 <> t2

  keys (Node k _ left right) = k : keys left <> keys right

  difference treeA Nil = treeA
  difference treeA (Node k _ left right) =
    difference newTreeA left <> difference newTreeA right
    where
      newTreeA = remove k treeA

  intersection treeA treeB = bstFromList int
    where
      int = intersection (inOrder treeA) (inOrder treeB)

instance (Ord k, Num k) => IsMap (WeightMap k v) where
  type MapValue (WeightMap k v) = v

  lookup k m = fmap snd $ leastLargest k m

  insertMap = insert

  deleteMap = remove

  mapFromList = bstFromList

  mapToList = inOrder

  singletonMap k v = insert k v Nil

instance BiPolyMap WeightMap where
  type BPMKeyConstraint WeightMap k = Ord k

  mapKeysWith fV fK m = mapKeysWith_ fV fK Nothing m

inOrder :: (Monoid (f (k, v)), Applicative f) => WeightMap k v -> f (k, v)
inOrder (Node k v left right) =
  inOrder left `mappend` pure (k, v) `mappend` inOrder right
inOrder Nil = mempty

remove :: Ord k => k -> WeightMap k v -> WeightMap k v
remove k (Node k' v left right)
  | k < k' = Node k' v (remove k left) right
  | k > k' = Node k' v left (remove k right)
  | otherwise = left <> right
remove k Nil = Nil

insert :: Ord k => k -> v -> WeightMap k v -> WeightMap k v
insert k v (Node k' v' left right)
  | k < k' = Node k' v' (insert k v left) right
  | k > k' = Node k' v' left (insert k v right)
  | otherwise = Node k v left right
insert k v Nil = Node k v Nil Nil

bstFromList :: Ord k => [(k, v)] -> WeightMap k v
bstFromList [] = Nil
bstFromList (x:xs) = uncurry insert x (bstFromList xs)

leastLargest :: Ord k => k -> WeightMap k v -> Maybe (k, v)
leastLargest k (Node k' v left right)
  | k < k' = case leastLargest k left of
               Nothing -> Just (k', v)
               val -> val
  | k > k' = leastLargest k right
  | otherwise = Just (k', v)
leastLargest k Nil = Nothing

mapKeysWith_ :: (Ord k1, Ord k2) => (v -> v -> v) -> (k1 -> k2) -> Maybe v -> WeightMap k1 v -> WeightMap k2 v
mapKeysWith_ fV fK (Just v1) (Node k v2 left right) = Node (fK k) (fV v1 v2) (newSubtree left) (newSubtree right)
  where
    newSubtree = mapKeysWith_ fV fK (Just v2)
mapKeysWith_ _ _ _ Nil = Nil
mapKeysWith_ fV fK Nothing (Node k v left right) = Node (fK k) v (newSubtree left) (newSubtree right)
  where
    newSubtree = mapKeysWith_ fV fK (Just v)

class (BiPolyMap w, IsMap (w k v)) => WeightedDict (w :: * -> * -> *) k v where
  lookupWeighted :: Ord k => k -> w k v -> Maybe (k, v)
  deleteWeighted :: Ord k => k -> w k v -> w k v
  updateWeights  :: Ord k => (k -> k) -> w k v -> w k v

instance (Num k, Ord k) => WeightedDict WeightMap k v where
  lookupWeighted = leastLargest

  deleteWeighted k m = fromMaybe m updatedWeights
    where
      k' = fmap fst $ leastLargest k m
      removed = remove <$> k' <*> pure m
      updatedWeights = mapKeysWith valId <$> (decrement <$> k') <*> removed
      valId _ x = x

  updateWeights f m = mapKeysWith valId f m
    where
      valId _ x = x

mRemoved :: (Num k, Ord k) => k -> WeightMap k v -> Maybe (v, WeightMap k v)
mRemoved k m = do
  oldVal <- lookup k m
  let m' = remove k m
  return $ (oldVal, m')
