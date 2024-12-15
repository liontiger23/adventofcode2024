{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle12
    ( puzzle12
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust)
import Data.List (nub, groupBy)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_)

puzzle12 :: Int -> Solution Integer
puzzle12 1 = solve1
puzzle12 2 = solve2

solve1 :: Solution Integer
solve1 = sum . map cost . regions . parseMap

solve2 :: Solution Integer
solve2 = undefined

----------------------------------------

-- >>> regions $ parseMap ["AAAA", "BBCD", "BBCC", "EEEC"]
-- [fromList [(0,0),(1,0),(2,0),(3,0)],fromList [(0,1),(0,2),(1,1),(1,2)],fromList [(0,3),(1,3),(2,3)],fromList [(2,1),(2,2),(3,2),(3,3)],fromList [(3,1)]]

regions :: Map -> [Region]
regions m = map S.fromList $ DS.equivalenceClasses $ execState collect $ DS.fromList $ M.keys m
 where
  collect :: State (DisjointSet Coordinate) ()
  collect = traverse_ update (M.keys m)
  update c = traverse_ (DS.unionS c) $ filter (sameRegion m c) $ adjacent c

-- >>> parseMap ["ABB", "BAB", "BBB"]
-- fromList [((0,0),'A'),((0,1),'B'),((0,2),'B'),((1,0),'B'),((1,1),'A'),((1,2),'B'),((2,0),'B'),((2,1),'B'),((2,2),'B')]

parseMap :: [String] -> Map
parseMap input = M.fromList [((x, y), c) |
     (y, l) <- zip [0..] input,
     (x, c) <- zip [0..] l
   ]

----------------------------------------

type Coordinate = (Int, Int)

type Map = M.Map Coordinate Char

type Region = S.Set Coordinate

sameRegion :: Map -> Coordinate -> Coordinate -> Bool
sameRegion m x y = M.lookup x m == M.lookup y m

cost :: Region -> Integer
cost r = area r * perimeter r

area :: Region -> Integer
area = fromIntegral . length

-- >>> perimeter $ S.fromList [(0,0)]
-- 4
--
-- >>> perimeter $ S.fromList [(0,0),(0,1)]
-- 6
--
-- >>> perimeter $ S.fromList [(0,0),(0,1),(1,1)]
-- 8
--

perimeter :: Region -> Integer
perimeter r = sum $ map count $ S.toList r
 where
  count c = fromIntegral $ length $ filter (`S.notMember` r) $ adjacent c

adjacent :: Coordinate -> [Coordinate]
adjacent (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

