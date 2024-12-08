{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle8
    ( puzzle8
    ) where

import Prelude hiding (elem)
import Util
import Data.Either (fromRight)
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe)
import Data.List (isPrefixOf, subsequences, sortBy, nub)
import Data.Map qualified as M
import Data.Set qualified as S
import Control.Monad (filterM)
import GHC.Base ((<|>))
import GHC.List (elem)
import Data.Char (isDigit)

puzzle8 :: Int -> Solution Int
puzzle8 1 = solve1
puzzle8 2 = solve2

solve1 :: Solution Int
solve1 = S.size . placeAntinodes . parseMap

solve2 :: Solution Int
solve2 = undefined

----------------------------------------

placeAntinodes :: Map -> S.Set Position
placeAntinodes m@(Map nx ny _) = S.fromList antinodes
 where
  antinodes = filter inbounds $ map (uncurry antinode) $ groupPairs m
  inbounds :: Position -> Bool
  inbounds (x, y) = 0 <= x && x < nx && 0 <= y && y < ny

groupPairs :: Map -> [(Position, Position)]
groupPairs (Map _ _ m) = concatMap (pairs . charPositions) $ nub $ M.elems m
 where
  charPositions :: Char -> [Position]
  charPositions c = M.keys $ M.filter (== c) m

-- >>> antinode (1,2) (2,3)
-- (0,1)
--
-- >>> antinode (2,3) (1,2)
-- (3,4)

antinode :: Position -> Position -> Position
antinode x y = x .+. x .-. y

-- >>> parseMap ["..0.",".a..","..a.","...0"]
-- Map 4 4 (fromList [((1,1),'a'),((2,0),'0'),((2,2),'a'),((3,3),'0')])

parseMap :: [String] -> Map
parseMap input = Map (length $ head input) (length input) charMap
 where
   charMap :: M.Map Position Char
   charMap = M.fromList [((x, y), c) |
       (y, l) <- zip [0..] input,
       (x, c) <- zip [0..] l,
       c /= '.'
     ]

----------------------------------------

type Position = (Int, Int)

-- >>> (1,1) .+. (2,2)
-- (3,3)
--
-- >>> (1,1) .+. (1,1) .-. (2,2)
-- (0,0)
-- 
-- >>> (2,2) .+. (2,2) .-. (1,1)
-- (3,3)

(.+.) :: Position -> Position -> Position
(a, b) .+. (c, d) = (a + c, b + d)

(.-.) :: Position -> Position -> Position
(a, b) .-. (c, d) = (a - c, b - d)

(.*.) :: Int -> Position -> Position
k .*. (a, b) = (k * a, k * b)

data Map = Map Int Int (M.Map Position Char)
  deriving (Show)

-- >>> pairs [1,2,3,4]
-- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

