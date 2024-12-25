{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle14
    ( puzzle14
    ) where

import Prelude hiding (elem)
import Util
import Data.Char (digitToInt, isDigit)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe, catMaybes, isNothing)
import Data.List (nub, groupBy, elem, sort, group, findIndex, isPrefixOf, tails)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn, splitWhen)

puzzle14 :: Int -> Solution Int
puzzle14 1 = solve1
puzzle14 2 = solve2

solve1 :: Solution Int
solve1 = product . map length . group . sort . mapMaybe (quadrant 101 103 . (!! 100) . uncurry (simulate 101 103) . parseCoordinates)

-- Will print infinitely
solve2 :: Solution Int
solve2 = length . show . map (debug "") . skip (12 : iterate (const 100) 100) . zip [0..] . slice 101 103 . map (uncurry (simulate 101 103) . parseCoordinates)

----------------------------------------

-- >>> skip (12 : iterate (const 100) 100) [0..1000]
-- [12,113,214,315,416,517,618,719,820,921]
-- >>> skip (88 : iterate (const 102) 102) [0..1000]
-- [88,191,294,397,500,603,706,809,912]

skip :: [Int] -> [a] -> [a]
skip [] xs = xs
skip (s : ss) xs = case drop s xs of
  []       -> []
  (x : xs') -> x : skip ss xs'

slice :: Integer -> Integer -> [[Coordinate]] -> [Map]
slice n m = map (Map n m) . transpose

-- >>> quadrant 11 7 (0, 0)
-- Just 1
-- >>> quadrant 11 7 (0, 3)
-- Nothing
-- >>> quadrant 11 7 (0, 4)
-- Just 2
-- >>> quadrant 11 7 (4, 0)
-- Just 1
-- >>> quadrant 11 7 (5, 0)
-- Nothing
-- >>> quadrant 11 7 (10, 6)
-- Just 4

quadrant :: Integer -> Integer -> Coordinate -> Maybe Int
quadrant n m (x, y)
  | x == midx || y == midy = Nothing
  | x <  midx              = Just $ if y < midy then 1 else 2
  | otherwise              = Just $ if y < midy then 3 else 4
 where
  midx = n `div` 2
  midy = m `div` 2

-- >>> take 10 $ simulate 11 7 (2, 4) (2, -3)
-- [(2,4),(4,1),(6,5),(8,2),(10,6),(1,3),(3,0),(5,4),(7,1),(9,5)]
--

simulate :: Integer -> Integer -> Coordinate -> Coordinate -> [Coordinate]
simulate n m p v = iterate step p
 where
  step :: Coordinate -> Coordinate
  step x = (x', y')
   where
    (nx, ny) = x .+. v
    x' = if 0 <= nx && nx < n then nx else abs (nx `mod` n)
    y' = if 0 <= ny && ny < m then ny else abs (ny `mod` m)

----------------------------------------

-- >>> parseCoordinates "p=0,4 v=3,-3"
-- ((0,4),(3,-3))
-- >>> parseInt "foo-123bar"
-- -123
-- >>> read "-123" :: Int
-- -123
-- >>> isDigitMinus '1'
-- True
--

parseCoordinates :: String -> (Coordinate, Coordinate)
parseCoordinates = parseSpan (/= ' ') parseCoordinate

parseCoordinate :: String -> Coordinate
parseCoordinate = parseSpan (/= ',') parseInt

parseSpan :: (Char -> Bool) -> (String -> a) -> String -> (a, a)
parseSpan p f s = let (x, y) = span p s in (f x, f y)

parseInt :: String -> Integer
parseInt = read . takeWhile isDigitMinus . dropWhile (not . isDigitMinus)

isDigitMinus :: Char -> Bool
isDigitMinus c = c `elem` ('-' : enumFromTo '0' '9')

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

data Map = Map Integer Integer [Coordinate]

instance Show Map where
  show = unlines . mapLines

mapLines :: Map -> [String]
mapLines (Map n m cs) = [[ch (x, y) | x <- [0..(n-1)]] | y <- [0..(m-1)]]
 where
  ch p
    | p `elem` cs = 'X'
    | otherwise   = '.'
