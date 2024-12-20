{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle20
    ( puzzle20
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

puzzle20 :: Int -> Solution Result
puzzle20 1 = solve1
puzzle20 2 = solve2

solve1 :: Solution Result
solve1 = length . filter (>= 100) . cheats . runWave . fromCharMap . parseCharMap

solve2 :: Solution Result
solve2 = undefined

type Result = Int

----------------------------------------

cheats :: Map -> [Int]
cheats m = concatMap cheatSavings $ M.keys m
 where
  cheatSavings p = map diff $ neighboursN 2 p
   where
    start = fromJust $ m ! p
    diff x = case M.lookup x m of
      Just (Just v)  -> v - start - 2
      _ -> 0

runWave :: (Map, Coordinate, Coordinate) -> Map
runWave (m, s, e) = execState (wave 0 [s]) m

wave :: Int -> [Coordinate] -> State Map ()
wave _ [] = pure ()
wave c ps = do
  mapM_ (\p -> modify (M.insert p $ Just c)) ps
  m <- get
  let ns = nub $ concatMap (filter ((== Just Nothing) . (`M.lookup` m)) . neighbours) ps
  wave (c + 1) ns

fromCharMap :: M.Map Coordinate Char -> (Map, Coordinate, Coordinate)
fromCharMap m = (M.map (const Nothing) $ M.filter (/= '#') m, start, end)
 where
  start = head $ M.keys $ M.filter (== 'S') m
  end = head $ M.keys $ M.filter (== 'E') m

-- >>> parseCharMap ["####", "#.E#", "#.S#", "####"]
-- fromList [((0,0),'#'),((0,1),'#'),((0,2),'#'),((0,3),'#'),((1,0),'#'),((1,1),'.'),((1,2),'.'),((1,3),'#'),((2,0),'#'),((2,1),'E'),((2,2),'S'),((2,3),'#'),((3,0),'#'),((3,1),'#'),((3,2),'#'),((3,3),'#')]

parseCharMap :: [String] -> M.Map Coordinate Char
parseCharMap input = M.fromList [((x, y), c) |
     (y, l) <- zip [0..] input,
     (x, c) <- zip [0..] l
   ]

----------------------------------------

type Coordinate = (Int, Int)

-- >>> (1,0) <= (1,1)
-- True
-- >>> (2,0) <= (1,1)
-- False

-- >>> (1,1) .+. (2,2)
-- (3,3)
--
-- >>> (1,1) .+. (1,1) .-. (2,2)
-- (0,0)
-- 
-- >>> (2,2) .+. (2,2) .-. (1,1)
-- (3,3)

(.+.) :: Coordinate -> Coordinate -> Coordinate
(a, b) .+. (c, d) = (a + c, b + d)

(.-.) :: Coordinate -> Coordinate -> Coordinate
(a, b) .-. (c, d) = (a - c, b - d)

dist2 :: Coordinate -> Coordinate -> Int
dist2 (a, b) (c, d) = abs (a - c) + abs (b - d)

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

-- >>> neighboursN 1 (0,0)
-- [(1,0),(-1,0),(0,1),(0,-1)]
-- 
-- >>> neighboursN 2 (0,0)
-- [(2,0),(1,1),(1,-1),(-2,0),(-1,1),(-1,-1),(1,1),(-1,1),(0,2),(1,-1),(-1,-1),(0,-2)]
--
-- >>> neighboursN 3 (0,0)
-- [(3,0),(2,1),(2,-1),(2,1),(1,2),(2,-1),(1,-2),(-3,0),(-2,1),(-2,-1),(-2,1),(-1,2),(-2,-1),(-1,-2),(2,1),(1,2),(-2,1),(-1,2),(1,2),(-1,2),(0,3),(2,-1),(1,-2),(-2,-1),(-1,-2),(1,-2),(-1,-2),(0,-3)]

neighboursN :: Int -> Coordinate -> [Coordinate]
neighboursN 1 p = neighbours p
neighboursN n p = concatMap (filter ((== n) . dist2 p) . neighboursN (n - 1 )) (neighbours p)

type Map = M.Map Coordinate (Maybe Int)

defaultMap :: Int -> Map
defaultMap n = M.fromList [((x, y), Nothing) | x <- [0..(n-1)], y <- [0..(n-1)]]

