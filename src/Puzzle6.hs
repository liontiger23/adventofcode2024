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
import Data.List (isPrefixOf, subsequences, sortBy, nub)
import Data.Map qualified as M
import Control.Monad (filterM)
import GHC.Base ((<|>))
import GHC.List (elem)

puzzle6 :: Int -> Solution Int
puzzle6 1 = solve1
puzzle6 2 = solve2

solve1 :: Solution Int
solve1 = length . M.filter visited . positions . run . parseMap

solve2 :: Solution Int
solve2 = length . filter (\s -> not (null s) && isJust (guard (last s))) . simulate . parseMap

----------------------------------------

run :: Map -> Map
run = last . iter

-- >>> pretty $ last $ iter $ parseMap ["..#.","#...","..^.","...#"]
-- ["..#.","#.+-","..|.","...#"]
-- >>> pretty $ last $ iter $ parseMap ["....","..#.",".#^#","..#."]
-- ["....","..#.",".#^#","..#."]

iter :: Map -> [Map]
iter cur@(Map m Nothing) = [cur]
iter cur@(Map m (Just (p, d))) = cur : case M.lookup p' m of
  Nothing -> iter $ Map m' Nothing
  Just EmptyValue -> iter $ Map m' $ Just (p', d)
  Just (Visited ds)
    | d `elem` ds -> [] -- cycle detected
    | otherwise   -> iter $ Map m' $ Just (p', d)
  Just Obstruction -> case M.lookup p m of
    Just (Visited ds) | d `elem` ds -> [] -- trivial cycle detected
    _ -> let d' = rotate d in iter $ Map m' $ Just (p, d')
 where
  p' = move p d
  m' = M.update mark p m
  mark EmptyValue   = Just $ Visited [d]
  mark (Visited ds) = Just $ Visited $ nub $ d : ds

simulate :: Map -> [[Map]]
simulate = map (concatMap iter . place) . iter
 where
  place :: Map -> [Map]
  place cur@(Map m Nothing) = []
  place cur@(Map m g@(Just (p, d))) = case M.lookup p' m of
    Nothing -> []
    Just EmptyValue -> [upd]
    Just (Visited _) -> []
    Just Obstruction -> []
   where
    p' = move p d
    upd = Map (M.update (const $ Just Obstruction) p' m) g

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

data Value = EmptyValue | Visited [Direction] | Obstruction
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
valueChar (Visited ds)
  | (u || d) && (r || l) = '+'
  |  u || d              = '|'
  |  r || l              = '-'
 where
  u = U `elem` ds
  r = R `elem` ds
  d = D `elem` ds
  l = L `elem` ds
valueChar Obstruction = '#'

directionChar :: Direction -> Char
directionChar U = '^'
directionChar R = '>'
directionChar D = 'V'
directionChar L = '<'

visited :: Value -> Bool
visited (Visited _) = True
visited _ = False
