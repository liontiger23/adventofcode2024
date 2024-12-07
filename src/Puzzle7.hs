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
findOps ops (Equation res vs) = safeHead $ filter (== res) $ gen vs
 where
  gen :: [Integer] -> [Integer]
  gen [] = undefined
  gen [x] = [x]
  gen (x : y : xs) = concatMap (gen . (: xs)) $ filter (<= res) $ map (evalOne x y) ops
  evalOne x y Mul = x * y
  evalOne x y Add = x + y
  evalOne x y Or  = read $ show x ++ show y

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
