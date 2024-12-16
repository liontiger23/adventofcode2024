{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle15
    ( puzzle15
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe)
import Data.List (nub, groupBy, elem)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_, foldlM)

puzzle15 :: Int -> Solution Int
puzzle15 1 = solve1
puzzle15 2 = solve2

solve1 :: Solution Int
solve1 = sum . map gps . M.keys . M.filter (== 'O') . uncurry simulate . parseInput

solve2 :: Solution Int
solve2 = undefined

----------------------------------------

gps :: Coordinate -> Int
gps (x, y) = y * 100 + x

simulate :: Map -> [Direction] -> Map
simulate m ds = execState steps m'
 where
  p = fst $ head $ M.toList $ M.filter (== '@') m
  m' = M.insert p '.' m
  steps = foldlM step p ds

step :: Coordinate -> Direction -> State Map Coordinate
step p d = do
  m <- get
  case shift m '.' (p .>. d) d of
    Nothing -> pure p -- $ debug ("[put " ++ show p ++ " " ++ [d] ++ " stop ]\n") p
    Just m' -> do
      put m' -- (debugWith ("[put " ++ show p ++ " " ++ [d] ++ " ]\n") (prettyWithPos $ p .>. d) m')
      pure (p .>. d)

shift :: Map -> Char -> Coordinate -> Direction -> Maybe Map
shift m c x d = --debug (pretty m ++ "\n" ++ [c] ++ show x) $
  case m ! x of
    '#' -> Nothing
    '.' -> Just $ M.insert x c m
    'O' -> shift (M.insert x c m) 'O' (x .>. d) d

line :: Coordinate -> Direction -> Map -> Map
line p d m = M.restrictKeys m $ S.fromList $ iterate (.>. d) p

parseInput :: [String] -> (Map, [Direction])
parseInput ss = let (m, d) = break null ss in (parseMap m, concat $ tail d)

-- >>> parseMap ["####", "#.0#", "#.@#", "####"]
-- fromList [((0,0),'#'),((0,1),'#'),((0,2),'#'),((0,3),'#'),((1,0),'#'),((1,1),'.'),((1,2),'.'),((1,3),'#'),((2,0),'#'),((2,1),'0'),((2,2),'@'),((2,3),'#'),((3,0),'#'),((3,1),'#'),((3,2),'#'),((3,3),'#')]

parseMap :: [String] -> Map
parseMap input = M.fromList [((x, y), c) |
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

type Direction = Char

-- >>> take 10 $ iterate (.>. '>') (1,1)
-- [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

(.>.) :: Coordinate -> Direction -> Coordinate
p .>. '>' = p .+. ( 1,  0)
p .>. '<' = p .+. (-1,  0)
p .>. '^' = p .+. ( 0, -1)
p .>. 'v' = p .+. ( 0,  1)

type Map = M.Map Coordinate Char

prettyWithPos :: Coordinate -> Map -> String
prettyWithPos p = pretty . M.insert p '@'

pretty :: Map -> String
pretty = unlines . mapLines

mapLines :: Map -> [String]
mapLines m = takeWhile (not . null) [[m ! (x, y) | x <- takeWhile (\x -> M.member (x, y) m) [0..]] | y <- [0..]]
