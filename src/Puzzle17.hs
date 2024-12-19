{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle17
    ( puzzle17
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt, isDigit)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe)
import Data.List (nub, groupBy, elem, minimumBy, intercalate)
import Control.Monad.State
import Control.Monad (void)
import Data.Foldable (traverse_, foldlM)
import Data.Function (on)
import Data.List.Split
import Data.Bits (xor)

puzzle17 :: Int -> Solution Result
puzzle17 1 = solve1
puzzle17 2 = solve2

solve1 :: Solution Result
solve1 = Result . evalState interpret . parseState

solve2 :: Solution Result
solve2 = undefined

----------------------------------------

-- >>> evalState interpret (MachineState {statePC=0, stateRegs=M.fromList [(A,729),(B,0),(C,0)], stateProg=[0,1,5,4,3,0]})
-- [4,6,3,5,6,3,5,2,1,0]

interpret :: State MachineState [Integer]
interpret = do
  pc <- gets statePC
  prog <- gets stateProg
  if pc >= length prog then
    pure []
  else do
    regs <- gets stateRegs
    let opcode = prog !! pc
        operand = prog !! (pc + 1)
        pc' = pc + 2
    case opcode of
      -- adv ~ A = A / 2^[combo operand]
      0 -> do
        op <- combo operand
        put (MachineState pc' (M.update (Just . (`div` (2^op))) A regs) prog)
        interpret
      -- bxl ~ B = B `xor` [literal operand]
      1 -> do
        op <- literal operand
        put (MachineState pc' (M.update (Just . (`xor` op)) B regs) prog)
        interpret
      -- bst ~ B = [combo operand] `mod` 8
      2 -> do
        op <- combo operand
        put (MachineState pc' (M.insert B (op `mod` 8) regs) prog)
        interpret
      -- jnz ~ if A /= 0 then pc = [literal operand]
      3 -> do
        op <- combo operand
        case regs ! A of
          0 -> put (MachineState pc' regs prog)
          _ -> put (MachineState (fromIntegral op) regs prog)
        interpret
      -- bxc ~ B = B `xor` C
      4 -> do
        put (MachineState pc' (M.update (Just . (`xor` (regs ! C))) B regs) prog)
        interpret
      -- out ~ print [combo operand] `mod` 8
      5 -> do
        op <- combo operand
        put (MachineState pc' regs prog)
        (op `mod` 8 :) <$> interpret
      -- bdv ~ B = A / 2^[combo operand]
      6 -> do
        op <- combo operand
        put (MachineState pc' (M.insert B (regs ! A `div` (2^op)) regs) prog)
        interpret
      -- cdv ~ C = A / 2^[combo operand]
      7 -> do
        op <- combo operand
        put (MachineState pc' (M.insert C (regs ! A `div` (2^op)) regs) prog)
        interpret

literal :: Int -> State MachineState Integer
literal x = pure $ fromIntegral x

combo :: Int -> State MachineState Integer
combo 4 = gets ((! A) . stateRegs)
combo 5 = gets ((! B) . stateRegs)
combo 6 = gets ((! C) . stateRegs)
combo 7 = undefined
combo x = pure $ fromIntegral x

----------------------------------------

parseState :: [String] -> MachineState
parseState input = MachineState 0 (M.fromList $ zip (enumFrom A) $ map parseInt regs) (map parseInt $ splitOn "," $ last prog)
 where
  (regs, prog) = break null input

-- >>> parseInt "foo123bar"
-- 123

parseInt :: Read a => String -> a
parseInt = read . takeWhile isDigit . dropWhile (not . isDigit)

----------------------------------------

data Register = A | B | C
  deriving (Show, Eq, Enum, Ord)

data MachineState = MachineState
  { statePC :: Int
  , stateRegs :: M.Map Register Integer
  , stateProg :: [Int]
  }
  deriving (Show)

newtype Result = Result [Integer]

instance Show Result where
  show (Result xs) = intercalate "," $ map show xs

