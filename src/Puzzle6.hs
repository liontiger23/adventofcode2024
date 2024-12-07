{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Puzzle6
    ( puzzle6
    ) where

import Prelude hiding (elem)
import Util
import Data.Either (fromRight)
import Data.Maybe (fromJust, isJust)
import Data.List (isPrefixOf, subsequences, sortBy)
import Data.Map qualified as M
import Control.Monad (filterM)
import GHC.Base ((<|>))

puzzle6 :: Int -> Solution Int
puzzle6 1 = solve1
puzzle6 2 = solve2

solve1 :: Solution Int
solve1 = length . M.filter (== Visited) . positions . run . parseMap

solve2 :: Solution Int
solve2 input = undefined

----------------------------------------

run :: Map -> Map
run = head . dropWhile (isJust . guard) . iterate step

-- >>> pretty $ step $ step $ parseMap ["..#.","#...","..^.","...#"]
-- ["..#.","#.X>","..X.","...#"]

step :: Map -> Map
step (Map m Nothing) = Map m Nothing
step (Map m (Just (p, d))) = case M.lookup p' m of
  Nothing -> Map m' Nothing
  Just EmptyValue -> Map m' $ Just (p', d)
  Just Visited -> Map m' $ Just (p', d)
  Just Obstruction -> let d' = rotate d in Map m' $ Just (move p d', d')
 where
  p' = move p d
  m' = M.update (const $ Just Visited) p m


-- >>> parseMap ["..#.","#...","..^.","...#"]
-- Map {obstructions = fromList [((0,0),EmptyValue),((0,1),Obstruction),((0,2),EmptyValue),((0,3),EmptyValue),((1,0),EmptyValue),((1,1),EmptyValue),((1,2),EmptyValue),((1,3),EmptyValue),((2,0),Obstruction),((2,1),EmptyValue),((2,3),EmptyValue),((3,0),EmptyValue),((3,1),EmptyValue),((3,2),EmptyValue),((3,3),Obstruction)], guard = Just ((2,2),U)}

parseMap :: [String] -> Map
parseMap input = Map (M.map charValue charMap) (Just (pos, U))
 where
   charMap :: M.Map Position Char
   charMap = M.fromList [((x, y), c) |
       (y, l) <- zip [0..] input,
       (x, c) <- zip [0..] l
     ]
   pos = head $ M.keys $ M.filter (== '^') charMap

----------------------------------------

type Position = (Int, Int)

data Value = EmptyValue | Visited | Obstruction
  deriving (Show, Eq)

data Direction = U | R | D | L
  deriving (Show, Eq, Enum)

data Map = Map
    { positions :: M.Map Position Value
    , guard :: Maybe (Position, Direction)
    }
  deriving Show

emptyMap :: Map
emptyMap = Map mempty Nothing

-- >>> map rotate [U,R,D,L]
-- [R,D,L,U]

rotate :: Direction -> Direction
rotate d = head $ tail $ dropWhile (/= d) dirs
 where dirs = L : enumFrom U

-- Note: upper left corner is (0, 0)
move :: Position -> Direction -> Position
move (x, y) U = (x, y - 1)
move (x, y) R = (x + 1, y)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)

pretty :: Map -> [String]
pretty (Map m g) = takeWhile (not . null)
  [ map fromJust (takeWhile isJust (map (\x -> lookupChar (x, y)) [0..]))
  | y <- [0..] ]
 where
  lookupChar :: Position -> Maybe Char
  lookupChar p = guardChar p <|> (valueChar <$> M.lookup p m)
  guardChar :: Position -> Maybe Char
  guardChar p = case g of
    Nothing -> Nothing
    Just (p', d) -> if p == p' then Just (directionChar d) else Nothing


charValue :: Char -> Value
charValue '.' = EmptyValue
charValue '^' = EmptyValue
charValue '#' = Obstruction

valueChar :: Value -> Char
valueChar EmptyValue  = '.'
valueChar Visited     = 'X'
valueChar Obstruction = '#'

directionChar :: Direction -> Char
directionChar U = '^'
directionChar R = '>'
directionChar D = 'V'
directionChar L = '<'
