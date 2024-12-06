{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Puzzle5
    ( puzzle5
    ) where

import Prelude hiding (elem)
import Util
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf, subsequences)

puzzle5 :: Int -> Solution Int
puzzle5 1 = solve1
puzzle5 2 = solve2

solve1 :: Solution Int
solve1 input = sum $ map mid $ filter (check rules) orders
 where (rules, orders) = readInput input

solve2 :: Solution Int
solve2 input = undefined

check :: [Rule] -> Order -> Bool
check rules order = all checkPair $ filter ((== 2) . length) $ subsequences order
 where
   checkPair [a,b] = not (any (wrongOrder a b) rules)
   wrongOrder a b (Rule x y) = a == y && b == x


readInput :: [String] -> ([Rule], [Order])
readInput input = (map parseRule ruleLines, map parseOrder (tail orderLines))
 where (ruleLines, orderLines) = break null input

------------------------------------

type Order = [Int]

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

data Rule = Rule Int Int
  deriving (Show, Eq)

type Parser s u m a = Stream s m Char => ParsecT s u m a

ruleParser :: Parser s u m Rule
ruleParser = Rule <$> intParser <* string "|" <*> intParser

intParser :: Parser s u m Int
intParser = read <$> many1 digit

parseRule :: String -> Rule
parseRule s = fromRight (error "rule parsing failed") $ parse ruleParser "parseRule" s

parseOrder :: String -> Order
parseOrder s = fromRight (error "order parsing failed") $ parse (sepBy intParser (char ',')) "parseOrder" s

