{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle10
    ( puzzle10
    ) where

import Prelude hiding (elem)
import Util
import Data.Char (digitToInt)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.List (nub, groupBy)

puzzle10 :: Int -> Solution Int
puzzle10 1 = solve1
puzzle10 2 = solve2

solve1 :: Solution Int
solve1 = length . nub . map shrink . fullTrails 0 10 . parseMap

solve2 :: Solution Int
solve2 = sum . map length . groupBy (\x y -> head x == head y) . fullTrails 0 10 . parseMap

----------------------------------------

-- >>> parseMap ["012", "123", "234"]
-- fromList [((0,0),0),((0,1),1),((0,2),2),((1,0),1),((1,1),2),((1,2),3),((2,0),2),((2,1),3),((2,2),4)]

parseMap :: [String] -> Map
parseMap input = M.fromList [((x, y), digitToInt c) |
     (y, l) <- zip [0..] input,
     (x, c) <- zip [0..] l
   ]

----------------------------------------

type Position = (Int, Int)

type Map = M.Map Position Int

type Trail = [Position]

adjacent :: Map -> Position -> [Position]
adjacent m (x, y) = filter (\p -> isJust $ M.lookup p m)
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- >>> trails (parseMap ["012", "123", "234"]) (0,1)
-- [[(0,1),(1,1),(2,1),(2,2)],[(0,1),(1,1),(1,2),(2,2)],[(0,1),(0,2),(1,2),(2,2)]]

trails :: Map -> Position -> [Trail]
trails m p
  | null nextPos = pure [p]
  | otherwise    = map (p :) $ concatMap (trails m) nextPos
 where nextPos = filter (\n -> m ! n == m ! p + 1) $ adjacent m p

-- >>> M.keys $ M.filter (== 0) (parseMap ["012", "123", "234"])
-- [(0,0)]
--
-- >>> let m = parseMap ["012", "123", "234"] in map (trails m) $ M.keys $ M.filter (== 0) m
-- [[[(0,0),(1,0),(2,0),(2,1),(2,2)],[(0,0),(1,0),(1,1),(2,1),(2,2)],[(0,0),(1,0),(1,1),(1,2),(2,2)],[(0,0),(0,1),(1,1),(2,1),(2,2)],[(0,0),(0,1),(1,1),(1,2),(2,2)],[(0,0),(0,1),(0,2),(1,2),(2,2)]]]
--
-- >>> fullTrails 0 5 (parseMap ["012", "123", "234"])
-- [[(0,0),(1,0),(2,0),(2,1),(2,2)],[(0,0),(1,0),(1,1),(2,1),(2,2)],[(0,0),(1,0),(1,1),(1,2),(2,2)],[(0,0),(0,1),(1,1),(2,1),(2,2)],[(0,0),(0,1),(1,1),(1,2),(2,2)],[(0,0),(0,1),(0,2),(1,2),(2,2)]]

fullTrails :: Int -> Int -> Map -> [Trail]
fullTrails s l m = concatMap (filter ((== l) . length) . trails m) (M.keys $ M.filter (== s) m)

shrink :: Trail -> (Position, Position)
shrink xs = (head xs, last xs)
