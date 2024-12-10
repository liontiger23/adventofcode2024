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
import Data.Map qualified as M

puzzle9 :: Int -> Solution Integer
puzzle9 1 = solve1
puzzle9 2 = solve2

solve1 :: Solution Integer
solve1 = checksum . compact . unpack . map digitToInt . head

solve2 :: Solution Integer
solve2 = checksum . unpackMap . compactLeft . packMap . map digitToInt . head

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
checksum xs = sum $ filter (> 0) $ zipWith (*) [0..] xs

----------------------------------------

-- >>> withGaps $ packMap $ map digitToInt "2333133121414131402"
-- [(0,(2,0)),(2,(3,-1)),(5,(3,1)),(8,(3,-1)),(11,(1,2)),(12,(3,-1)),(15,(3,3)),(18,(1,-1)),(19,(2,4)),(21,(1,-1)),(22,(4,5)),(26,(1,-1)),(27,(4,6)),(31,(1,-1)),(32,(3,7)),(35,(1,-1)),(36,(4,8)),(40,(2,9))]
-- >>> unpackMap $ packMap $ map digitToInt "2333133121414131402"
-- [0,0,-1,-1,-1,1,1,1,-1,-1,-1,2,-1,-1,-1,3,3,3,-1,4,4,-1,5,5,5,5,-1,6,6,6,6,-1,7,7,7,-1,8,8,8,8,9,9]
-- >>> unpackMap $ compactLeft $ packMap $ map digitToInt "2333133121414131402"
-- [0,0,9,9,2,1,1,1,7,7,7,-1,4,4,-1,3,3,3,-1,-1,-1,-1,5,5,5,5,-1,6,6,6,6,-1,-1,-1,-1,-1,8,8,8,8]

packMap :: [Int] -> M.Map Int (Int, Integer)
packMap = M.fromList . packMap' 0 0
 where
  packMap' i k [x] = [(i, (x, k))]
  packMap' i k (x:y:xs) = (i, (x, k)) : packMap' (i + x + y) (k + 1) xs

unpackMap :: M.Map Int (Int, Integer) -> [Integer]
unpackMap = concatMap (uncurry replicate . snd) . withGaps

withGaps :: M.Map Int (Int, Integer) -> [(Int, (Int, Integer))]
withGaps = fillGaps 0 . M.toList
 where
  fillGaps :: Int -> [(Int, (Int, Integer))] -> [(Int, (Int, Integer))]
  fillGaps _ [] = []
  fillGaps prev ((i, (k, v)): xs)
    | prev < i  = (prev, (i - prev, -1)) : (i, (k, v)) : fillGaps (i + k) xs
    | otherwise = (i, (k, v)) : fillGaps (i + k) xs

compactLeft :: M.Map Int (Int, Integer) -> M.Map Int (Int, Integer)
compactLeft initM = compactLeft' back initM
 where
  back = reverse $ M.toList initM
  compactLeft' :: [(Int, (Int, Integer))] -> M.Map Int (Int, Integer) -> M.Map Int (Int, Integer)
  compactLeft' [] m = m
  compactLeft' ((i, (k, v)) : xs) m = case safeHead (filter (\(j, (n, w)) -> w < 0 && j < i && n >= k) $ withGaps m) of
    Nothing          -> compactLeft' xs m
    Just (j, (n, w))
      | n == k -> compactLeft' xs $ M.delete i $ M.insert j (k, v) m
      | n >  k -> compactLeft' xs $ M.delete i $ M.insert j (k, v) $ M.insert (j + k) (n - k, w) m
