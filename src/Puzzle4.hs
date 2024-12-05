{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Puzzle4
    ( puzzle4
    ) where

import Prelude hiding (elem)
import Util
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

puzzle4 :: Int -> Solution Int
puzzle4 1 = solve1
puzzle4 2 = undefined

solve1 :: Solution Int
solve1 xxs = sum $ map collect (coords m)
 where
  m = makeMatrix xxs
  collect :: (Int, Int) -> Int
  collect (i, j) = length $ filter (isPrefixOf "XMAS") $ map (elems m i j) directions


data Matrix a = Matrix
    { numCols :: Int
    , numRows :: Int
    , rows ::[[a]]
    }
  deriving (Show, Eq)

coords :: Matrix a -> [(Int, Int)]
coords (Matrix n m _) = [(i,j) | i <- [0..n-1], j <- [0..m-1]]

elem :: Matrix a -> Int -> Int -> Maybe a
elem (Matrix n m xxs) i j
  | 0 <= i && i < n &&
    0 <= j && j < m = Just $ (xxs !! j) !! i
  | otherwise       = Nothing

elems :: Matrix a -> Int -> Int -> Direction -> [a]
elems m i j d =
  let (x,  y ) = directionCoords d
  in case elem m i j of
       Nothing -> []
       Just a  -> a : elems m (i+x) (j+y) d

makeMatrix :: [String] -> Matrix Char
makeMatrix xxs = Matrix (length $ head xxs) (length xxs) xxs

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Enum)

directions :: [Direction]
directions = enumFrom N

-- Note: (0,0) coordinates are top-left (north-west corner)
directionCoords :: Direction -> (Int, Int)
directionCoords N  = ( 0, -1)
directionCoords NE = ( 1, -1)
directionCoords E  = ( 1,  0)
directionCoords SE = ( 1,  1)
directionCoords S  = ( 0,  1)
directionCoords SW = (-1,  1)
directionCoords W  = (-1,  0)
directionCoords NW = (-1, -1)


