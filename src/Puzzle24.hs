{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle24
    ( puzzle24
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

puzzle24 :: Int -> Solution Result
puzzle24 1 = solve1
puzzle24 2 = solve2

solve1 :: Solution Result
solve1 = toInt . uncurry eval . parseInput

solve2 :: Solution Result
solve2 = undefined

type Result = Int

----------------------------------------

type Wire = String

type InputMap = M.Map Wire Bool

data Gate = AND | OR | XOR
  deriving (Show, Read, Eq)

data Connection = Connection Wire Gate Wire
  deriving (Show)

type ConnectionMap = M.Map Wire Connection

type ValueMap = M.Map Wire Bool

----------------------------------------

eval :: InputMap -> ConnectionMap -> ValueMap
eval i c = execState (mapM_ evalS (filter endWire $M.keys c)) M.empty
 where
  evalS :: Wire -> State ValueMap Bool
  evalS w
    | M.member w i = pure (i ! w)
    | otherwise    = do
      m <- get
      case M.lookup w m of
        Just v  -> pure v
        Nothing -> do
          let (Connection a g b) = c ! w
          av <- evalS a
          bv <- evalS b
          let v = case g of
                AND -> av &&  bv
                OR  -> av ||  bv
                XOR -> av .^. bv
          modify (M.insert w v)
          pure v

toInt :: ValueMap -> Int
toInt m = foldr (flip setBit . wireNum) 0 $ M.keys (M.filterWithKey (\ w b -> b && endWire w) m)

wireNum :: Wire -> Int
wireNum = read . tail

endWires :: M.Map Wire a -> [Wire]
endWires = filter endWire . M.keys

endWire :: Wire -> Bool
endWire = (== 'z') . head

----------------------------------------

-- Parsing

parseInput :: Input -> (InputMap, ConnectionMap)
parseInput input = (M.fromList (map parseWireValue ws), M.fromList $ map parseConnection (tail cs))
 where
  (ws, cs) = break null input

parseWireValue :: String -> (Wire, Bool)
parseWireValue s = (init w, v == "1")
 where
  [w, v] = words s

parseConnection :: String -> (Wire, Connection)
parseConnection s = (c, Connection a (read g) b)
 where
  [a, g, b, "->", c] = words s


