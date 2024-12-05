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
puzzle4 2 = solve2

solve1 :: Solution Int
solve1 xxs = sum $ map collect (coords m)
 where
  m = makeMatrix xxs
  collect :: (Int, Int) -> Int
  collect (i, j) = length $ filter (isPrefixOf "XMAS") $ map (elems m i j) directions

solve2 :: Solution Int
solve2 xxs = sum $ map collect (coords m)
 where
  m = makeMatrix xxs
  collect :: (Int, Int) -> Int
  collect (i, j) = if num >= 2 then 1 else 0
   where num = length $ filter (isPrefixOf "MAS") $ diags m i j

diags :: Matrix a -> Int -> Int -> [[a]]
diags m i j = map (diag m i j) [NW, NE, SE, SW]

diag :: Matrix a -> Int -> Int -> Direction -> [a]
diag m i j d =
  let (x, y) = shift i j d
  in  elems m x y (opposite d)

opposite :: Direction -> Direction
opposite NW = SE
opposite NE = SW
opposite SE = NW
opposite SW = NE


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
  let (i', j') = shift i j d
  in case elem m i j of
       Nothing -> []
       Just a  -> a : elems m i' j' d

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

shift :: Int -> Int -> Direction -> (Int, Int)
shift i j d =
  let (x, y) = directionCoords d
  in  (i + x, j + y)


