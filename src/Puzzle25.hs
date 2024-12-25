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
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix, intercalate, intersperse, partition)
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
solve1 = Part1 . length . filter (uncurry (fit 5)) . pairs . parseSchematics

solve2 :: Solution Result
solve2 = Part2 . undefined

data Result = Part1 Int | Part2 Int

instance Show Result where
  show (Part1 x) = show x
  show (Part2 x) = show x

----------------------------------------

data SchematicType = Lock | Key
  deriving (Show, Eq)

data Schematic = Schematic SchematicType [Int]
  deriving (Show, Eq)

isLock :: Schematic -> Bool
isLock (Schematic t _) = t == Lock

isKey :: Schematic -> Bool
isKey = not . isLock

-- >>> parseSchematic ["##",".#"]
-- Lock [1,2]

parseSchematic :: [String] -> Schematic
parseSchematic ss
  | head (head ss) == '#' = Schematic Lock heights
  | head (head ss) == '.' = Schematic Key heights
 where
  heights = map (subtract 1 . length . filter (== '#')) $ transpose ss

parseSchematics :: [String] -> [Schematic]
parseSchematics ss = map parseSchematic $ splitOn [""] ss

----------------------------------------

-- >>> pairs [Lock [1], Key [2], Key [3]]
-- [(Lock [1],Key [2]),(Lock [1],Key [3])]

pairs :: [Schematic] -> [(Schematic, Schematic)]
pairs xs = [(lock, key) | lock <- locks, key <- keys]
 where (locks, keys) = partition isLock xs

-- >>> overlap 5 (Schematic Lock [0,5,3,4,3]) (Schematic Key [5,0,2,1,3])
-- True

overlap :: Int -> Schematic -> Schematic -> Bool
overlap n (Schematic _ xs) (Schematic _ ys) = any (> n) $ zipWith (+) xs ys

fit :: Int -> Schematic -> Schematic -> Bool
fit n a b = not $ overlap n a b


