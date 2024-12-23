{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle23
    ( puzzle23
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe, isNothing, fromJust)
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix, intercalate)
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM, asum)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down (Down))
import Data.Bits
import Data.Int

puzzle23 :: Int -> Solution Result
puzzle23 1 = solve1
puzzle23 2 = solve2

solve1 :: Solution Result
solve1 = Part1 . S.size . S.filter (any ((== 't') . head) . S.elems) . (!! 3) . subnets . parseNetwork

solve2 :: Solution Result
solve2 = Part2 . S.elems . maxSubnet . parseNetwork

data Result = Part1 Int | Part2 [Node]

instance Show Result where
  show (Part1 x) = show x
  show (Part2 xs) = intercalate "," xs

----------------------------------------

maxSubnet :: Network -> S.Set Node
maxSubnet = S.elemAt 0 . last . takeWhile (not . S.null) . subnets

-- >>> take 3 $ subnets $ parseNetwork ["tc-th","ab-tc","th-ab","th-gy"]
-- [fromList [fromList ["ab"],fromList ["gy"],fromList ["tc"],fromList ["th"]],fromList [fromList ["ab","tc"],fromList ["ab","th"],fromList ["gy","th"],fromList ["tc","th"]],fromList [fromList ["ab","tc","th"]]]
--
-- >>> neighbours (parseNetwork ["tc-th","ab-tc","th-ab","th-gy"]) (S.fromList ["ab"])
-- ["tc","th"]
--
-- >>> parseNetwork ["tc-th","ab-tc","th-ab","th-gy"]
-- fromList [("ab",fromList ["tc","th"]),("gy",fromList ["th"]),("tc",fromList ["ab","th"]),("th",fromList ["ab","gy","tc"])]

subnets :: Network -> [S.Set (S.Set Node)]
subnets m = subnets' singletons
 where
  singletons = S.map S.singleton $ M.keysSet m
  subnets' cur = cur : subnets' (S.fromList $ concatMap expand $ S.elems cur)
   where
    expand :: S.Set Node -> [S.Set Node]
    expand s = map (`S.insert` s) $ neighbours m s


neighbours :: Network -> S.Set Node -> [Node]
neighbours m s = S.elems $ foldr (S.intersection . (m !)) (M.keysSet m) (S.elems s)


parseNetwork :: [String] -> Network
parseNetwork input = execState (mapM (fill . parseConnection) input) M.empty
 where
  fill :: Connection -> State Network ()
  fill (x, y) = modify (M.insertWith S.union x (S.singleton y) . M.insertWith S.union y (S.singleton x))

-- >>> parseConnection "tc-th"
-- ("tc","th")

parseConnection :: String -> Connection
parseConnection (a1 : a2 : _ : b1 : b2 : _) = ([a1, a2], [b1, b2])

----------------------------------------

type Node = String

type Connection = (Node, Node)

type Network = M.Map Node (S.Set Node)

connected :: Network -> Node -> Node -> Bool
connected net x y = maybe False (S.member y) $ M.lookup x net
