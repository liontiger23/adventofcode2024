{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle22
    ( puzzle22
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe, isNothing, fromJust)
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix)
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM, asum)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down (Down))
import Data.Bits
import Data.Int

puzzle22 :: Int -> Solution Result
puzzle22 1 = solve1
puzzle22 2 = solve2

solve1 :: Solution Result
solve1 = sum . map (fromIntegral . (!! 2000) . gen . read)

solve2 :: Solution Result
solve2 = maxSum . map (take 2001 . map digit1 . gen . read)

type Result = Integer

----------------------------------------

maxSum :: [[Int8]] -> Integer
maxSum xxs = maximum $ M.elems mem
 where mem = M.unionsWith (+) $ map (\xs -> M.map fromIntegral $ calcSequences M.empty (changes $ take 5 xs) (xs !! 4) (drop 5 xs)) xxs

type Mem = M.Map [Int8] Int8

calcSequences :: Mem -> [Int8] -> Int8 -> [Int8] -> Mem
calcSequences m k v [] = M.insertWith (\_ o -> o) k v m
calcSequences m k v (x : xs) = calcSequences m' (tail k ++ [x - v]) x xs
 where m' = M.insertWith (\_ o -> o) k v m

changes :: [Int8] -> [Int8]
changes [x] = []
changes (x : y : xs) = (y - x) : changes (y : xs)

digit1 :: Int -> Int8
digit1 x = fromIntegral (x `mod` 10)


-- >>> take 100 $ map digit1 $ gen 123
-- [3,0,6,5,4,4,6,4,4,2,4,0,4,0,3,9,1,4,5,6,6,3,4,6,5,6,9,4,6,4,7,4,7,6,0,8,7,5,9,3,2,7,0,2,0,3,0,5,1,6,3,6,7,9,3,9,2,0,5,8,9,3,2,7,4,9,5,5,2,5,6,6,8,0,0,8,5,3,7,5,6,8,4,5,9,3,0,2,2,3,9,8,7,6,4,7,4,0,3,1]
--
-- >>> take 100 $ changes $ map digit1 $ gen 123
-- [-3,6,-1,-1,0,2,-2,0,-2,2,-4,4,-4,3,6,-8,3,1,1,0,-3,1,2,-1,1,3,-5,2,-2,3,-3,3,-1,-6,8,-1,-2,4,-6,-1,5,-7,2,-2,3,-3,5,-4,5,-3,3,1,2,-6,6,-7,-2,5,3,1,-6,-1,5,-3,5,-4,0,-3,3,1,0,2,-8,0,8,-3,-2,4,-2,1,2,-4,1,4,-6,-3,2,0,1,6,-1,-1,-1,-2,3,-3,-4,3,-2,-1]

----------------------------------------

-- >>> (gen 1) !! 2000
-- 8685429
--
-- >>> take 10 $ gen 123
-- [123,15887950,16495136,527345,704524,1553684,12683156,11100544,12249484,7753432]

gen :: Int -> [Int]
gen x = x : gen (genOne x)

-- >>> genOne 123
-- 15887950

genOne :: Int -> Int
genOne = step3 . step2 . step1
 where
  step1 x = prune $ mix x (x .<<. 6)
  step2 x = prune $ mix x (x .>>. 5)
  step3 x = prune $ mix x (x .<<. 11)

mix :: Int -> Int -> Int
mix x y = x .^. y

prune :: Int -> Int
prune x = x .&. mask

mask :: Int
mask = (1 .<<. 24) - 1
