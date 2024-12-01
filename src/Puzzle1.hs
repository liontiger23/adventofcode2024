{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle1
    ( puzzle1
    ) where

import Util
import Data.Char ( digitToInt, isDigit )
import Data.List ( isPrefixOf, elemIndex, sort )
import Data.Maybe ( fromJust )

puzzle1 :: Int -> Solution Int
puzzle1 1 = sum . calcDistances . sortLists . lists
puzzle1 2 = sum . calcSimilarities . sortLists . lists

lists :: [String] -> ([Int], [Int])
lists [] = ([], [])
lists (x : xs) = (l : ls, r : rs)
 where
  (ls, rs) = lists xs
  [l, r] = map read $ words x

distance x y = abs (x - y)

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (ls,rs) = (sort ls, sort rs)

calcDistances :: ([Int], [Int]) -> [Int]
calcDistances ([], [])    = []
calcDistances (l:ls,r:rs) = distance l r : calcDistances (ls,rs)

calcSimilarities :: ([Int], [Int]) -> [Int]
calcSimilarities ([], _) = []
calcSimilarities (_, []) = []
calcSimilarities (l:ls, r:rs)
    | l == r =
      let (cl, ls') = gobble l (l:ls)
          (cr, rs') = gobble l (r:rs)
      in l * cl * cr : calcSimilarities (ls', rs')
    | l < r     = calcSimilarities (ls, r:rs)
    | otherwise = calcSimilarities (l:ls, rs)

gobble :: Int -> [Int] -> (Int, [Int])
gobble p xs =
  let (prefix,rest) = span (== p) xs
  in (length prefix, rest)
