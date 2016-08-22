{-# LANGUAGE OverloadedStrings #-}

module Util where

import Prelude ()
import ClassyPrelude

import Data.Array

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