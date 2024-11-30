{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle1
    ( puzzle1
    ) where

import Util
import Data.Char ( digitToInt, isDigit )
import Data.List ( isPrefixOf, elemIndex )
import Data.Maybe ( fromJust )

puzzle1 :: Int -> Solution Int
puzzle1 1 = collect $ map digitToInt . filter isDigit
puzzle1 2 = collect readExtraDigits

collect :: (String -> [Int]) -> Solution Int
collect parse input = sum $ map (nums . parse) input
 where nums xs = head xs * 10 + last xs

readExtraDigits :: String -> [Int]
readExtraDigits [] = []
readExtraDigits s@(c : cs)
  | isDigit c = digitToInt c : readExtraDigits cs
  | otherwise = map (\d -> succ $ fromJust $ elemIndex d digits) (filter (`isPrefixOf` s) digits) ++ readExtraDigits cs

digits :: [String]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
