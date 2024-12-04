{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Puzzle3
    ( puzzle3
    ) where

import Util
import Text.Parsec
import Data.Either (fromRight)

puzzle3 :: Int -> Solution Int
puzzle3 1 = sum . map eval . muls . concat
puzzle3 2 = undefined

data Mul = Mul Int Int
  deriving (Show, Eq)

eval :: Mul -> Int
eval (Mul x y) = x * y

mulParser :: Stream s m Char => ParsecT s u m Mul
mulParser = (Mul . read <$> (string "mul(" *> upto1 3 digit)) <*> (read <$> (string "," *> upto1 3 digit <* string ")"))

mulsParser :: Stream s m Char => ParsecT s u m [Mul]
mulsParser = ((:) <$> try mulParser <*> mulsParser)
         <|> try (anyChar *> mulsParser)
         <|> return []

muls :: String -> [Mul]
muls s = fromRight [] $ parse mulsParser "" s

upto :: Int -> ParsecT s u m a -> ParsecT s u m [a]
upto n p
  | n > 0     = (:) <$> try p <*> upto (n-1) p
            <|> return []
  | otherwise = return []

upto1 :: Int -> ParsecT s u m a -> ParsecT s u m [a]
upto1 n p
  | n > 0     = (:) <$> p <*> upto (n-1) p
  | otherwise = return []
