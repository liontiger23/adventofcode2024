{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle19
    ( puzzle19
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
import Control.Monad (void)
import Data.Foldable (traverse_, foldlM, asum)
import Data.Function (on)
import Data.List.Split (splitOn)
import Text.Parsec
import Data.Ord (comparing, Down (Down))

puzzle19 :: Int -> Solution Result
puzzle19 1 = solve1
puzzle19 2 = solve2

solve1 :: Solution Result
solve1 = length . filter (not . null) . uncurry mapVariants . parseInput

solve2 :: Solution Result
solve2 = undefined

type Result = Int

----------------------------------------

mapVariants :: [Pattern] -> [String] -> [[[Pattern]]]
mapVariants ps = map (variants ps)

variants :: [Pattern] -> String -> [[Pattern]]
variants _ [] = [[]]
variants ps s = concat $ zipWith variants' ps $ map (`stripPrefix` s) ps
 where
  variants' p Nothing   = []
  variants' p (Just s') = map (p :) $ variants ps s'

parseInput :: Input -> ([Pattern], [String])
parseInput input =
  let (ps, ss) = break null input
  in  (splitOn ", " $ head ps, tail ss)

----------------------------------------

type Pattern = String
