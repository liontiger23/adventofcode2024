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

puzzle22 :: Int -> Solution Result
puzzle22 1 = solve1
puzzle22 2 = solve2

solve1 :: Solution Result
solve1 = sum . map (fromIntegral . (!! 1999) . gen . read)

solve2 :: Solution Result
solve2 = undefined

type Result = Integer

----------------------------------------

----------------------------------------

-- >>> (gen 1) !! 1999
-- 8685429
--
-- >>> take 10 $ gen 123
-- [15887950,16495136,527345,704524,1553684,12683156,11100544,12249484,7753432,5908254]

gen :: Int -> [Int]
gen x = let next = genOne x in next : gen next

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
