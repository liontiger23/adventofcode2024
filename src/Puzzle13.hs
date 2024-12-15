{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

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

puzzle13 :: Int -> Solution Integer
puzzle13 1 = solve1
puzzle13 2 = solve2

solve1 :: Solution Integer
solve1 = sum . mapMaybe (fmap cost . solution) . parseGames

solve2 :: Solution Integer
solve2 = sum . mapMaybe (fmap cost . solution . adjust) . parseGames

----------------------------------------

-- For each game need to solve following equations in integers for (an, bn):
--   ax*an + bx*bn = px
--   ay*an + by*bn = py
--
-- After simplification:
--
--   an = (py*bx - px*by) / (bx*ay - by*ax)
--   bn = (px*ay - py*ax) / (bx*ay - by*ax)

solution :: Game -> Maybe (Integer, Integer)
solution (Game (ax, ay) (bx, by) (px, py))
  | anum `mod` denom == 0 && bnum `mod` denom == 0 = Just (anum `div` denom, bnum `div` denom)
  | otherwise = Nothing
 where
  denom = bx * ay - by * ax
  anum = py * bx - px * by
  bnum = px * ay - py * ax

adjust :: Game -> Game
adjust (Game a b p) = Game a b (10000000000000 .*. (1, 1) .+. p)

cost :: (Integer, Integer) -> Integer
cost (a, b) = 3 * a + b

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

parseInt :: String -> Integer
parseInt = read . takeWhile isDigit . dropWhile (not . isDigit)

----------------------------------------

type Coordinate = (Integer, Integer)

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

(.*.) :: Integer -> Coordinate -> Coordinate
k .*. (x, y) = (k * x, k * y)

data Game = Game
    { buttonA :: Coordinate
    , buttonB :: Coordinate
    , prize   :: Coordinate
    }
  deriving (Show, Eq)

