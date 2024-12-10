{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle9
    ( puzzle9
    ) where

import Prelude hiding (elem)
import Util
import Data.Char (digitToInt)

puzzle9 :: Int -> Solution Integer
puzzle9 1 = solve1
puzzle9 2 = solve2

solve1 :: Solution Integer
solve1 = checksum . compact . unpack . map digitToInt . head

solve2 :: Solution Integer
solve2 = undefined

----------------------------------------

unpack :: [Int] -> [Integer]
unpack = unpack' 0
 where
  unpack' k [x] = replicate x k
  unpack' k (x:y:xs) = replicate x k ++ replicate y (-1) ++ unpack' (k + 1) xs

compact :: [Integer] -> [Integer]
compact lst = compact' left right
 where
  left  = zip [0..] lst
  right = reverse left
  compact' :: [(Int, Integer)] -> [(Int, Integer)] -> [Integer]
  compact' lls@((l, lv) : ls) rrs@((r, rv) : rs)
    | l > r      = []
    | lv >= 0    = lv : compact' ls rrs
    | rv >= 0    = rv : compact' ls (dropWhile ((< 0) . snd) rs)
    | otherwise  = compact' lls (dropWhile ((< 0) . snd) rs)

checksum :: [Integer] -> Integer
checksum xs = sum $ zipWith (*) [0..] xs
