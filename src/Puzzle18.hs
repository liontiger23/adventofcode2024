{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle18
    ( puzzle18
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
import Data.List (nub, groupBy, elem, minimumBy)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_, foldlM)
import Data.Function (on)

puzzle18 :: Int -> Solution Int
puzzle18 1 = solve1
puzzle18 2 = solve2

bound = 71

solve1 :: Solution Int
solve1 = fromJust . (! (bound - 1, bound - 1)) . execState (wave 0 [(0, 0)]) . markBytes (defaultMap bound) . take 1024 . parseBytes

solve2 :: Solution Int
solve2 = undefined

----------------------------------------

wave :: Int -> [Coordinate] -> State Map ()
wave _ [] = pure ()
wave c ps = do
  mapM_ (\p -> modify (M.insert p $ Just c)) ps
  m <- get
  let ns = nub $ concatMap (filter ((== Just Nothing) . (`M.lookup` m)) . neighbours) ps
  wave (c + 1) ns


markBytes :: Map -> [Coordinate] -> Map
markBytes = foldr M.delete

parseBytes :: [String] -> [Coordinate]
parseBytes = map parseCoordinate

parseCoordinate :: String -> Coordinate
parseCoordinate s = (read x, read $ tail y)
 where (x, y) = break (== ',') s

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

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

type Map = M.Map Coordinate (Maybe Int)

defaultMap :: Int -> Map
defaultMap n = M.fromList [((x, y), Nothing) | x <- [0..(n-1)], y <- [0..(n-1)]]

