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
solve2 = run . fmap sum . mapM (blinkN 75 . read) . words . head
 where run s = evalState s M.empty

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

type Memo a = State (M.Map (Integer, Integer) Integer) a

blinkN :: Integer -> Integer -> Memo Integer
blinkN 0 _ = pure 1
blinkN n x = get >>= \m -> maybe calc pure (M.lookup (n, x) m)
 where
  calc = do
    let rs = blink [x]
    res <- sum <$> mapM (blinkN (n - 1)) rs
    modify $ M.insert (n, x) res
    pure res

