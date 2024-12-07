{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle5
    ( puzzle5
    ) where

import Prelude hiding (elem)
import Util
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf, subsequences, sortBy)
import Data.Set (Set)
import Data.Set qualified as Set

puzzle5 :: Int -> Solution Int
puzzle5 1 = solve1
puzzle5 2 = solve2

solve1 :: Solution Int
solve1 input = sum $ map mid $ filter (check rules) orders
 where (rules, orders) = readInput input

solve2 :: Solution Int
solve2 input = undefined

check :: Rules -> Order -> Bool
check rules order = order == reorder rules order

reorder :: Rules -> Order -> Order
reorder rules = sortBy (ruleOrdering rules)

ruleOrdering :: Rules -> Int -> Int -> Ordering
ruleOrdering rules x y
  | x == y = EQ
  | Set.member (x, y) rules = LT
  | Set.notMember (x, y) rules = GT
  | otherwise = LT



readInput :: [String] -> (Rules, [Order])
readInput input = (Set.fromList $ map parseRule ruleLines, map parseOrder (tail orderLines))
 where (ruleLines, orderLines) = break null input

------------------------------------

type Order = [Int]

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

type Rules = Set Rule
type Rule = (Int, Int)

type Parser s u m a = Stream s m Char => ParsecT s u m a

ruleParser :: Parser s u m Rule
ruleParser = (,) <$> intParser <* string "|" <*> intParser

intParser :: Parser s u m Int
intParser = read <$> many1 digit

parseRule :: String -> Rule
parseRule s = fromRight (error "rule parsing failed") $ parse ruleParser "parseRule" s

parseOrder :: String -> Order
parseOrder s = fromRight (error "order parsing failed") $ parse (sepBy intParser (char ',')) "parseOrder" s

