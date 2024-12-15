{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle13
    ( puzzle13
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt, isDigit)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Data.List (nub, groupBy, elem, sort)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn, splitWhen)

puzzle13 :: Int -> Solution Int
puzzle13 1 = solve1
puzzle13 2 = solve2

solve1 :: Solution Int
solve1 = sum . mapMaybe (safeHead . sort . map cost . filter (<= (100, 100)) . solutions) . parseGames

solve2 :: Solution Int
solve2 = undefined

----------------------------------------

cost :: (Int, Int) -> Int
cost (a, b) = 3 * a + b

-- >>> solutions $ parseGame ["Button A: X+94, Y+34", "Button B: X+22, Y+67", "Prize: X=8400, Y=5400"]
-- [(80,40)]

solutions :: Game -> [(Int, Int)]
solutions (Game a b p) = [(an, bn) | bn <- fill b p, an <- fill a (p .-. (bn .*. b)), ((an .*. a) .+. (bn .*. b)) == p]
 where
  fill c n = takeWhile (\k -> k .*. c <= n) [0..]

----------------------------------------

-- >>> parseGame ["Button A: X+94, Y+34", "Button B: X+22, Y+67", "Prize: X=8400, Y=5400"]
-- Game {buttonA = (94,34), buttonB = (22,67), prize = (8400,5400)}
-- >>> parseInt "foo123bar"
-- 123

parseGames :: [String] -> [Game]
parseGames = map parseGame . splitWhen null

parseGame :: [String] -> Game
parseGame [a, b, p] = Game (parseCoordinate a) (parseCoordinate b) (parseCoordinate p)

parseCoordinate :: String -> Coordinate
parseCoordinate s = let (x, y) = span (/= ',') s in (parseInt x, parseInt y)

parseInt :: String -> Int
parseInt = read . takeWhile isDigit . dropWhile (not . isDigit)

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

(.*.) :: Int -> Coordinate -> Coordinate
k .*. (x, y) = (k * x, k * y)

data Game = Game
    { buttonA :: Coordinate
    , buttonB :: Coordinate
    , prize   :: Coordinate
    }
  deriving (Show, Eq)

