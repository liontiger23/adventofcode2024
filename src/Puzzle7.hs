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

puzzle7 :: Int -> Solution Int
puzzle7 1 = solve1
puzzle7 2 = solve2

solve1 :: Solution Int
solve1 = sum . mapMaybe (findOps . parseEquation)

solve2 :: Solution Int
solve2 = undefined

----------------------------------------

findOps :: Equation -> Maybe Int
findOps (Equation res xs) = safeHead $ filter (== res) $ map (eval xs) $ gen (length xs - 1)
 where
  gen :: Int -> [[Op]]
  gen 1 = [[Add], [Mul]]
  gen n =
    let next = gen (n - 1)
    in  map (Add :) next ++ map (Mul :) next

----------------------------------------

data Equation = Equation Int [Int]
  deriving (Show, Eq)

-- >>> parseEquation "123: 1 2 3"
-- Equation 123 [1,2,3]

parseEquation :: String -> Equation
parseEquation s = Equation r xs
 where (r : xs) = map (read . takeWhile isDigit) $ words s

data Op = Mul | Add
  deriving (Show, Eq)

-- >>> eval [1,2,3] [Add,Mul]
-- 9
-- >>> eval [1,2,3] [Mul,Add]
-- 5

eval :: [Int] -> [Op] -> Int
eval [x] _ = x
eval (x : y : xs) (op : ops) =
  let v = case op of
            Mul -> x * y
            Add -> x + y
  in eval (v : xs) ops
eval []  _ = undefined
eval xs [] = undefined
