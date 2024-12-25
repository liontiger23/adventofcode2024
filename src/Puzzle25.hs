{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle25
    ( puzzle25
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
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix, intercalate, intersperse)
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM, asum, foldrM)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down (Down))
import Data.Bits
import Data.Int
import Data.Tuple (swap)

puzzle25 :: Int -> Solution Result
puzzle25 1 = solve1
puzzle25 2 = solve2

solve1 :: Solution Result
solve1 = Part1 . undefined

solve2 :: Solution Result
solve2 = Part2 . undefined

data Result = Part1 Int | Part2 Int

instance Show Result where
  show (Part1 x) = show x
  show (Part2 x) = show x

----------------------------------------

