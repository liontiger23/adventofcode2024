{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Puzzle17
    ( puzzle17
    ) where

import Prelude hiding (elem)
import DisjointSet (DisjointSet)
import DisjointSet qualified as DS
import Util
import Data.Char (digitToInt, isDigit)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (isJust, mapMaybe)
import Data.List (nub, groupBy, elem, minimumBy, intercalate, isPrefixOf, isSuffixOf, foldl')
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM)
import Data.Function (on)
import Data.List.Split
import Data.Bits (xor, shift, shiftR, Bits (..))

puzzle17 :: Int -> Solution Result
puzzle17 1 = solve1
puzzle17 2 = solve2

solve1 :: Solution Result
solve1 = Result . evalState interpret . parseState

solve2 :: Solution Result
solve2 = Result . (: []) . minimum . (\ops -> backtrack 0 ops M.empty) . stateProg . parseState

-- For second part we have to rely on hard-coded formula from the input:
--
--   b   = (a mod 8) xor 7
--   out = (b xor (a div (2 ^ b)) xor 7) mod 8
--   a   = a div 8
--
-- which can be simplified to this
--
--   out = (a mod 8) xor (a >> (7 - (a mod 8)) mod 8)
--   a   = a >> 3
--

----------------------------------------

-- >>> backtrack 0 [2,1,4,0,7,4,0,2,3] M.empty
-- [6849153376,2570963296,62769504,578668896,58575200,6849153392,2570963312,62769520,578668912,58575216,6849153368,2570963288,62769496,578668888,58575192,6849153396,2570963316,62769524,578668916,58575220,2701410036,2569289460,2702982900,2570862324,62701300,62686210,2701410033,2569289457,2702982897,2570862321,62701297,6849153373,2570963293,62769501,578668893,58575197]

backtrack :: Int -> [Int] -> Mem -> [Integer]
backtrack _ [] m = [memToInteger m]
backtrack p (x : xs) m = concatMap (backtrack (p + 3) xs) $ concatMap xors $ candidates p m
 where
  xors bs = map newMem $ filter (\as -> x == toInt (bs `xor'` as)) $ candidates p' m'
   where
    m' = foldr (uncurry M.insert) m $ zip [p..] bs
    p' = p + 7 - toInt bs
    newMem as = foldr (uncurry M.insert) m' $ zip [p'..] as
  xor' = zipWith xor
  candidates i m' = mapMaybe (matches $ lookup3 i m') [[a,b,c] | a <- [False,True], b <- [False,True], c <- [False,True]]
  matches = zipWithM matches1
  matches1 Nothing  b = Just b
  matches1 (Just a) b
    | a == b    = Just b
    | otherwise = Nothing


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
        ((op `mod` 8) :) <$> interpret
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

type Mem = M.Map Int Bool

memToInteger :: Mem -> Integer
memToInteger m =
  let addBit x set acc = if set then acc `setBit` x else acc `clearBit` x
  in foldr (uncurry addBit) 0 $ M.toList m

lookup3 :: Int -> Mem -> [Maybe Bool]
lookup3 x m = map (\i -> M.lookup (x + i) m) [0,1,2]

-- >>> toInt [True,False,True]
-- 5
-- >>> toInt [True]
-- 1

toInt :: [Bool] -> Int
toInt = toInt' 0 0
 where
  toInt' acc _ [] = acc
  toInt' acc i (x : xs) =
    let acc' = if x then acc `setBit` i else acc `clearBit` i
    in  toInt' acc' (i + 1) xs

--bits3 :: Int -> Mem -> Maybe Int
--bits3 x m = set1 0 $ set1 1 $ set1 2 0
-- where
--  set1 i acc = case M.lookup (x + i) m of
--    Nothing -> Nothing
--    Just v -> if v then acc `setBit` i else acc `clearBit` i

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

