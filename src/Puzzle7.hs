{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle7
    ( puzzle7
    ) where

import Prelude hiding (elem)
import Util
import Data.Either (fromRight)
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe)
import Data.List (isPrefixOf, subsequences, sortBy, nub)
import Data.Map qualified as M
import Control.Monad (filterM)
import GHC.Base ((<|>))
import GHC.List (elem)
import Data.Char (isDigit)

puzzle7 :: Int -> Solution Integer
puzzle7 1 = solve1
puzzle7 2 = solve2

solve1 :: Solution Integer
solve1 = sum . mapMaybe (findOps [Add, Mul] . parseEquation)

solve2 :: Solution Integer
solve2 = sum . mapMaybe (findOps [Add, Mul, Or] . parseEquation)

----------------------------------------

findOps :: [Op] -> Equation -> Maybe Integer
findOps ops (Equation res xs) = safeHead $ filter (== res) $ mapMaybe (eval res xs) $ gen (length xs - 1)
 where
  gen :: Int -> [[Op]]
  gen 1 = map (: []) ops
  gen n = [ op : gops | gops <- gen (n - 1), op <- ops ]

----------------------------------------

data Equation = Equation Integer [Integer]
  deriving (Show, Eq)

-- >>> parseEquation "123: 1 2 3"
-- Equation 123 [1,2,3]

parseEquation :: String -> Equation
parseEquation s = Equation r xs
 where (r : xs) = map (read . takeWhile isDigit) $ words s

data Op = Mul | Add | Or
  deriving (Show, Eq)

-- >>> eval [1,2,3] [Add,Mul]
-- 9
-- >>> eval [1,2,3] [Mul,Add]
-- 5

eval :: Integer -> [Integer] -> [Op] -> Maybe Integer
eval _ [x] _ = Just x
eval limit (x : y : xs) (op : ops) =
  let v = case op of
            Mul -> x * y
            Add -> x + y
            Or  -> read $ show x ++ show y
  in if limit < v then Nothing else eval limit (v : xs) ops
eval _ []  _ = undefined
eval _ xs [] = undefined
