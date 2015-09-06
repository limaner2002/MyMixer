{-# LANGUAGE OverloadedStrings #-}

module Tabular
    ( renderRows
    , Row
    ) where
import Data.List (transpose)

type Row = [String]

sample =
    [ ["Rock", "44"]
    , ["Solid Gold Oldies", "2"]
    ]

colPad = 2

columnLengths :: [Row] -> [Int]
columnLengths =
    (fmap (maximum . (fmap length))) . transpose

renderRows :: [Row] -> String
renderRows rows =
    foldr (\x y -> x ++ "\n" ++ y) "" paddedRows
  where
    paddedRows = fmap (\(l, r) -> renderRow l r) $ zip lengths rows
    lengths = repeat $ columnLengths rows

renderRow :: [Int] -> Row -> String
renderRow lengths row =
    foldr (++) "" padded
  where
    padded = fmap (\(l, r) -> pad l r) $ zip lengths row

pad :: Int -> String -> String
pad maxLength str =
    str ++ replicate ((maxLength+colPad) - l) ' '
  where
    l = length str

