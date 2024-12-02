{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle2
    ( puzzle2
    ) where

import Util
import Data.Char ( digitToInt, isDigit )
import Data.List ( isPrefixOf, elemIndex, sort )
import Data.Maybe ( fromJust )

puzzle2 :: Int -> Solution Int
puzzle2 1 = length . filter safe . map (diffs . ints)
puzzle2 2 = undefined

ints :: String -> [Int]
ints = map read . words

diffs :: [Int] -> [Int]
diffs []       = []
diffs [x]      = []
diffs (x:y:xs) = y - x : diffs (y:xs)

safe :: [Int] -> Bool
safe xs = (all (> 0) xs || all (< 0) xs) && all ((<= 3) . abs) xs

