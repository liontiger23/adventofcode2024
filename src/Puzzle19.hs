{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle19
    ( puzzle19
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
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix)
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM, asum)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down (Down))

puzzle19 :: Int -> Solution Result
puzzle19 1 = solve1
puzzle19 2 = solve2

solve1 :: Solution Result
solve1 = fromIntegral . length . filter (/= 0) . uncurry mapVariants . parseInput

solve2 :: Solution Result
solve2 = sum . uncurry mapVariants . parseInput

type Result = Integer

----------------------------------------

mapVariants :: [Pattern] -> [String] -> [Integer]
mapVariants ps ss = evalState (mapM (variants ps) ss) M.empty

variants :: [Pattern] -> String -> State Mem Integer
variants _ [] = pure 1
variants ps s = do
  m <- get
  case M.lookup s m of
    Just v  -> pure v
    Nothing -> do
      res <- sum <$> mapM (variants ps) (mapMaybe (`stripPrefix` s) ps)
      modify (M.insert s res)
      pure res

parseInput :: Input -> ([Pattern], [String])
parseInput input =
  let (ps, ss) = break null input
  in  (splitOn ", " $ head ps, tail ss)

----------------------------------------

type Pattern = String

type Mem = M.Map String Integer
