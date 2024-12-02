{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle2
    ( puzzle2
    ) where

import Util
import Data.Char ( digitToInt, isDigit )
import Data.List ( isPrefixOf, elemIndex, sort, elemIndices )
import Data.Maybe ( fromJust )

puzzle2 :: Int -> Solution Int
puzzle2 1 = length . filter safe  . map (diffs . ints)
puzzle2 2 = length . filter (safe' True) . map ints

ints :: String -> [Int]
ints = map read . words

diffs :: [Int] -> [Int]
diffs []       = []
diffs [x]      = []
diffs (x:y:xs) = y - x : diffs (y:xs)

safe :: [Int] -> Bool
safe xs = (all (> 0) xs || all (< 0) xs) && all ((<= 3) . abs) xs

safe' :: Bool -> [Int] -> Bool
safe' False xs' = (all (> 0) xs || all (< 0) xs) && all ((<= 3) . abs) xs
 where xs = diffs xs'
safe' True xs = safe' False xs || any (safe' False) (drops xs)

drops :: [Int] -> [[Int]]
drops [] = []
drops (x:xs) = xs : map (x:) (drops xs)
