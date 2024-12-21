{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle21
    ( puzzle21
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

puzzle21 :: Int -> Solution Result
puzzle21 1 = solve1
puzzle21 2 = solve2

solve1 :: Solution Result
solve1 = sum . map (complexity 3)

solve2 :: Solution Result
solve2 = sum . map (complexity 26)

type Result = Integer

----------------------------------------

-- >>> complexity 3 "029A"
-- 1972
-- >>> complexity 3 "980A"
-- 58800
-- >>> complexity 3 "179A"
-- 12172
-- >>> complexity 3 "456A"
-- 29184
-- >>> complexity 3 "379A"
-- 24256

complexity :: Int -> [Char] -> Integer
complexity n cs = read (init cs) * moveLength n cs

-- >>> moveSequence 1 "029A"
-- [L,A,U,A,U,U,R,A,D,D,D,A]
-- >>> moveSequence 2 "029A"
-- [D,L,L,A,R,R,U,A,L,A,R,A,L,A,A,D,R,A,U,A,L,D,A,A,A,U,R,A]
-- >>> moveSequence 3 "029A"
-- [L,D,A,L,A,A,R,R,U,A,D,A,A,L,U,A,R,A,D,L,L,A,R,R,U,A,D,A,U,A,D,L,L,A,R,R,U,A,A,L,D,A,R,A,U,A,L,A,R,A,D,L,L,A,R,A,U,R,A,A,A,L,A,D,R,A,U,A]
-- >>> moveSequence 3 "379A"
-- [D,L,L,A,R,R,U,A,D,A,U,A,L,D,A,L,A,A,R,R,U,A,A,D,A,L,U,A,R,A,A,D,A,U,A,L,D,A,U,R,A,A,L,A,R,A,D,L,L,A,R,A,U,R,A,A,A,L,A,D,R,A,U,A]

moveSequence :: Int -> [Char] -> [Move]
moveSequence n cs = moveSequence' n
 where
  numMap = numericKeymap
  dirMap = directionalKeymap
  moveSequence' 1 = getMoves numMap ('A' : cs)
  moveSequence' k = getMoves dirMap (A : moveSequence' (k - 1))

-- >>> moveLength 1 "029A"
-- 12
-- >>> moveLength 2 "029A"
-- 28
-- >>> moveLength 3 "029A"
-- 68
-- >>> moveLength 3 "379A"
-- 64

moveLength :: Int -> [Char] -> Integer
moveLength n cs = evalState (moveLength' n (A : getMoves numericKeymap ('A' : cs))) M.empty

moveLength' :: Int -> [Move] -> State Mem Integer
moveLength' n [x] = pure 0
moveLength' n (x : y : xs) = (+) <$> move1Length' n x y <*> moveLength' n (y : xs)

move1Length' :: Int -> Move -> Move -> State Mem Integer
move1Length' 1 _ _ = pure 1
move1Length' n x y = do
  m <- get
  case M.lookup ((x, y), n) m of
    Just v  -> pure v
    Nothing -> do
      v <- moveLength' (n - 1) $ A : directionalKeymap ! (x, y)
      modify (M.insert ((x, y), n) v)
      pure v

----------------------------------------

getMoves :: Ord a => Keymap a -> [a] -> [Move]
getMoves _ [x] = []
getMoves m (x : y : xs) = m ! (x, y) ++ getMoves m (y : xs)

numericKeymap :: Keymap Char
numericKeymap = keymap numericKeypad

numericKeypad :: Keypad Char
numericKeypad = keypad
  [ map Just ['7', '8', '9']
  , map Just ['4', '5', '6']
  , map Just ['1', '2', '3']
  , [Nothing, Just '0', Just 'A']
  ]

directionalKeymap :: Keymap Move
directionalKeymap = keymap directionalKeypad

directionalKeypad :: Keypad Move
directionalKeypad = keypad
  [ [Nothing, Just U, Just A]
  , map Just [L, D, R]
  ]

-- >>> getMoves directionalKeymap [A,D,L,A]
-- [L,D,A,L,A,R,R,U,A]
-- >>> getMoves directionalKeymap [A,L,D,A]
-- [D,L,L,A,R,A,U,R,A]

-- >>> keymap directionalKeypad
-- fromList [((L,L),[A]),((L,D),[R,A]),((L,U),[R,U,A]),((L,R),[R,R,A]),((L,A),[R,R,U,A]),((D,L),[L,A]),((D,D),[A]),((D,U),[U,A]),((D,R),[R,A]),((D,A),[U,R,A]),((U,L),[D,L,A]),((U,D),[D,A]),((U,U),[A]),((U,R),[D,R,A]),((U,A),[R,A]),((R,L),[L,L,A]),((R,D),[L,A]),((R,U),[L,U,A]),((R,R),[A]),((R,A),[U,A]),((A,L),[D,L,L,A]),((A,D),[L,D,A]),((A,U),[L,A]),((A,R),[D,A]),((A,A),[A])]

keymap :: Ord a => Keypad a -> Keymap a
keymap m = M.fromList [((a, b), minimum (moves x y) ++ [A]) | (x, a) <- M.toList m, (y, b) <- M.toList m]
 where
  moves :: Coordinate -> Coordinate -> [[Move]]
  moves a@(ax, ay) b@(bx, by)
    | a == b = [[]]
    | ax /= bx && ay /= by = concatMap (\c -> [ xs ++ ys | xs <- moves a c, ys <- moves c b]) corners
    | ax < bx && M.member (ax + 1, ay) m = map (R :) $ moves (ax + 1, ay) b
    | ax > bx && M.member (ax - 1, ay) m = map (L :) $ moves (ax - 1, ay) b
    | ay < by && M.member (ax, ay + 1) m = map (D :) $ moves (ax, ay + 1) b
    | ay > by && M.member (ax, ay - 1) m = map (U :) $ moves (ax, ay - 1) b
   where
    corners = filter (`M.member` m) [(bx, ay), (ax, by)]

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

data Move = L | D | U | R | A
  deriving (Show, Eq, Ord, Enum)

type Keypad a = M.Map Coordinate a

type Keymap a = M.Map (a, a) [Move]

-- >>> keypad [[Just 1, Just 2],[Nothing, Just 0]]
-- fromList [((0,0),1),((1,0),2),((1,1),0)]

keypad :: [[Maybe a]] -> Keypad a
keypad vvs = M.fromList [((x,y), v) | (y, vs) <- zip [0..] vvs, (x, Just v) <- zip [0..] vs]

type Mem = M.Map ((Move, Move), Int) Integer
