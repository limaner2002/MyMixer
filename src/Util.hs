{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import ClassyPrelude
import ParseCSV

import Data.Array
import Data.Aeson

editDifference :: String -> String -> Int
editDifference strA strB = d lenA lenB
    where
      d 0 i = i
      d j 0 = j
      d i j
          | arrA ! i == arrB ! j = ds ! (i-1, j-1)
          | otherwise = minimumEx
              [ (ds ! (i-1, j)) + 1
              , (ds ! (i, j-1)) + 1
              ]

      ds = listArray bounds [d i j | (i, j) <- range bounds]

      bounds = ((0,0), (lenA, lenB))
      (lenA, lenB) = (length strA, length strB)
      (arrA, arrB) = (listArray (1,lenA) strA, listArray (1,lenB) strB)

class Diffable a where
    closestMatch :: a -> [a] -> Maybe (a, Int)

-- closestMatch' :: String -> [String] -> Maybe (String, Int)
-- closestMatch' target results = minimumByMay minDiff $ zip results differenceList
--     where
--       differenceList = fmap (editDifference target) results
--       minDiff (_, a) (_, b) = compare a b

diff :: [String] -> [String] -> Int
diff targetParts resultParts = foldl' (+) 0 diffs
    where
      diffs = fmap diff $ zip targetParts resultParts
      diff (target, result) = editDifference target result

fromJSON' :: (MonadThrow m, FromJSON a) => Value -> m a 
fromJSON' v = case fromJSON v of
  Error e -> throwM $ ParseError (e <> ": " <> (unpack . decodeUtf8 . encode $ v))
  Success x -> return x
