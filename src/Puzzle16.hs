{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle16
    ( puzzle16
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
import Data.List (nub, groupBy, elem, minimumBy)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_, foldlM)
import Data.Function (on)

puzzle16 :: Int -> Solution Integer
puzzle16 1 = solve1
puzzle16 2 = solve2

solve1 :: Solution Integer
solve1 = run1 . fromCharMap . parseCharMap

solve2 :: Solution Integer
solve2 = run2 . fromCharMap . parseCharMap

----------------------------------------

-- >>> (\(m, s, e) -> findPaths s m) $ fromCharMap $ parseCharMap ["####", "#E.#", "#.S#", "####"]
-- fromList [((1,1),fromList [(N,3002),(E,4002),(S,3002),(W,2002)]),((1,2),fromList [(N,3001),(E,4001),(S,3001),(W,2001)]),((2,1),fromList [(N,1001),(E,2001),(S,3001),(W,2001)]),((2,2),fromList [(N,1000),(E,0),(S,1000),(W,2000)])]
--
-- >>> backtrack (1,1) W $ (\(m, s, e) -> findPaths s m) $ fromCharMap $ parseCharMap ["####", "#E.#", "#.S#", "####"]
-- [[(1,1),(2,1),(2,2)]]

run1 :: (Map, Coordinate, Coordinate) -> Integer
run1 (m, s, e) = minimum $ M.elems $ findPaths s m ! e

run2 :: (Map, Coordinate, Coordinate) -> Integer
run2 (m, s, e) = fromIntegral $ length $ nub $ concat $ concatMap (\d -> backtrack e d m') ds
 where
  m' = findPaths s m
  c = minimum $ M.elems (m' ! e)
  ds = M.keys $ M.filter (== c) (m' ! e)

backtrack :: Coordinate -> Direction -> Map -> [[Coordinate]]
backtrack p d m
  | null paths = [[p]]
  | otherwise  = map (p:) paths
 where
  paths = concatMap backtrackDirection directionsToTry
  ds = m ! p
  c = ds ! d
  directionsToTry = d : filter ((== c - 1000) . (ds !)) [clockwise d, counterclockwise d]
  backtrackDirection d' = case M.lookup p' m of
    Nothing -> []
    Just ds'
      | ds' ! d' == (ds ! d') - 1 -> backtrack p' d' m
      | otherwise -> []
   where
    p' = p .>. invert d'

findPaths :: Coordinate -> Map -> Map
findPaths s m = execState (bfs (+ 1) (+ 1000) s) $ M.insert s (M.singleton E 0) m

bfs :: (Integer -> Integer) -> (Integer -> Integer) -> Coordinate -> State Map [Coordinate]
bfs step rotate p = do
  m <- get
  let ds = minimize rotate (m ! p)
  put $ M.insert p ds m
  let next d = do
        m' <- get
        case stepOne (step (ds ! d)) (p .>. d) d m' of
          Nothing  -> pure []
          Just m'' -> do
            put m''
            bfs step rotate (p .>. d)
  concat <$> mapM next (M.keys ds)

stepOne :: Integer -> Coordinate -> Direction -> Map -> Maybe Map
stepOne c p d m = case M.lookup p m of
  Nothing -> Nothing
  Just ds  -> case M.lookup d ds of
    Nothing -> Just $ M.insert p (M.insert d c ds) m
    Just v 
      | v > c     -> Just $ M.update (Just . M.insert d c) p m
      | otherwise -> Nothing

-- >>> minimize (+ 1000) $ M.fromList [(E, 0), (W, 10)]
-- fromList [(N,1000),(E,0),(S,1000),(W,10)]

minimize :: (Integer -> Integer) -> M.Map Direction Integer -> M.Map Direction Integer
minimize cost m =
  M.insertWith min (clockwise d)             (cost c) $
  M.insertWith min (counterclockwise d)      (cost c) $
  M.insertWith min (clockwise $ clockwise d) (cost $ cost c) m
 where
  (d, c) = minimumBy (compare `on` snd) $ M.toList m

fromCharMap :: M.Map Coordinate Char -> (Map, Coordinate, Coordinate)
fromCharMap m = (M.map (const M.empty) $ M.filter (/= '#') m, start, end)
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

data Direction = N | E | S | W
  deriving (Show, Eq, Ord)

clockwise :: Direction -> Direction
clockwise N = E
clockwise E = S
clockwise S = W
clockwise W = N

counterclockwise :: Direction -> Direction
counterclockwise S = E
counterclockwise W = S
counterclockwise N = W
counterclockwise E = N

invert :: Direction -> Direction
invert = clockwise . clockwise

-- >>> take 10 $ iterate (.>. E) (1,1)
-- [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

(.>.) :: Coordinate -> Direction -> Coordinate
p .>. W = p .+. (-1,  0)
p .>. E = p .+. ( 1,  0)
p .>. N = p .+. ( 0, -1)
p .>. S = p .+. ( 0,  1)

type Map = M.Map Coordinate (M.Map Direction Integer)
