{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle11
    ( puzzle11
    ) where

import Prelude hiding (elem)
import Util
import Data.Char (digitToInt)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.List (nub, groupBy)
import Control.Monad.State

puzzle11 :: Int -> Solution Integer
puzzle11 1 = solve1
puzzle11 2 = solve2

solve1 :: Solution Integer
solve1 = fromIntegral . length . (!! 25) . iterate blink . map read . words . head

solve2 :: Solution Integer
solve2 = sum . map (memoizedBlink 75 . read) . words . head

----------------------------------------

-- >>> blink [125,17]
-- [253000,1,7]
--
-- >>> blink $ blink [125,17]
-- [253,0,2024,14168]
--
-- >>> blink [1000]
-- [10,0]

blink :: [Integer] -> [Integer]
blink [] = []
blink (x : xs)
  | x == 0 = 1 : blink xs
  | even (length str) = read l : read r : blink xs
  | otherwise = x * 2024 : blink xs
 where
  str = show x
  (l, r) = splitAt (length str `div` 2) str

-- Stolen from https://stackoverflow.com/a/44508289

type Memo = State (M.Map (Int, Integer) Integer) Integer

memoizedBlink :: Int -> Integer -> Integer
memoizedBlink n d = evalState (blink' (n,d)) M.empty
 where
  memoo :: ((Int, Integer) -> Memo) -> (Int, Integer) -> Memo
  memoo f x = gets (M.lookup x) >>= maybe b' return
   where
    b' = do
      b <- f x
      modify $ M.insert x b
      return b
  blink' (0,_) = return 1
  blink' (n,x)
    | x == 0 = memoo blink' ((n - 1),1)
    | even (length str) = (+) <$> memoo blink' ((n - 1),(read l)) <*> memoo blink' ((n - 1),(read r))
    | otherwise = memoo blink' ((n - 1),(x * 2024))
   where
    str = show x
    (l, r) = splitAt (length str `div` 2) str
