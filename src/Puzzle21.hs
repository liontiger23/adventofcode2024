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
solve2 = undefined

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
complexity n cs = read (init cs) * fromIntegral (minimum $ map length $ moveSequence n cs)

-- >>> minimumBy (comparing length) $ moveSequence 1 "029A"
-- [L,A,U,A,R,U,U,A,D,D,D,A]
-- >>> minimumBy (comparing length) $ moveSequence 2 "029A"
-- [D,L,L,A,R,R,U,A,L,A,R,A,D,A,L,U,A,A,R,A,L,D,A,A,A,R,U,A]
-- >>> minimumBy (comparing length) $ moveSequence 3 "029A"
-- [L,D,A,L,A,A,R,R,U,A,D,A,A,L,U,A,R,A,D,L,L,A,R,R,U,A,D,A,U,A,L,D,A,R,U,A,D,L,L,A,R,U,A,R,A,A,D,A,U,A,D,L,L,A,R,A,R,U,A,A,A,D,A,L,U,A,R,A]
-- >>> minimumBy (comparing length) $ moveSequence 3 "379A"
-- [D,L,L,A,R,R,U,A,D,A,U,A,L,D,A,L,A,A,R,R,U,A,A,D,A,L,U,A,R,A,A,D,A,U,A,L,D,A,R,U,A,A,L,A,R,A,D,L,L,A,R,A,R,U,A,A,A,D,A,L,U,A,R,A]

moveSequence :: Int -> [Char] -> [[Move]]
moveSequence 1 cs = getMoves numericKeymap ('A' : cs)
moveSequence k cs = concatMap (getMoves directionalKeymap . (A :)) (moveSequence (k - 1) cs)

----------------------------------------

getMoves :: Ord a => Keymap a -> [a] -> [[Move]]
getMoves _ [x] = [[]]
getMoves m (x : y : xs) = concatMap (\ys -> [ns ++ ys| ns <- m ! (x, y)]) $ getMoves m (y : xs)

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
-- [[L,D,A,L,A,R,R,U,A],[D,L,A,L,A,R,R,U,A]]
-- >>> getMoves directionalKeymap [A,L,D,A]
-- [[D,L,L,A,R,A,R,U,A],[D,L,L,A,R,A,U,R,A]]

-- >>> keymap directionalKeypad
-- fromList [((U,U),[[A]]),((U,D),[[D,A]]),((U,L),[[D,L,A]]),((U,R),[[R,D,A],[D,R,A]]),((U,A),[[R,A]]),((D,U),[[U,A]]),((D,D),[[A]]),((D,L),[[L,A]]),((D,R),[[R,A]]),((D,A),[[R,U,A],[U,R,A]]),((L,U),[[R,U,A]]),((L,D),[[R,A]]),((L,L),[[A]]),((L,R),[[R,R,A]]),((L,A),[[R,R,U,A]]),((R,U),[[L,U,A],[U,L,A]]),((R,D),[[L,A]]),((R,L),[[L,L,A]]),((R,R),[[A]]),((R,A),[[U,A]]),((A,U),[[L,A]]),((A,D),[[L,D,A],[D,L,A]]),((A,L),[[D,L,L,A]]),((A,R),[[D,A]]),((A,A),[[A]])]

keymap :: Ord a => Keypad a -> Keymap a
keymap m = M.fromList [((a, b), map (++ [A]) $ moves x y) | (x, a) <- M.toList m, (y, b) <- M.toList m]
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

data Move = U | D | L | R | A
  deriving (Show, Eq, Ord, Enum)

type Keypad a = M.Map Coordinate a

type Keymap a = M.Map (a, a) [[Move]]

-- >>> keypad [[Just 1, Just 2],[Nothing, Just 0]]
-- fromList [((0,0),1),((1,0),2),((1,1),0)]

keypad :: [[Maybe a]] -> Keypad a
keypad vvs = M.fromList [((x,y), v) | (y, vs) <- zip [0..] vvs, (x, Just v) <- zip [0..] vs]
