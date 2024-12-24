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
import Data.List (nub, groupBy, elem, minimumBy, inits, sort, sortBy, stripPrefix, intercalate, intersperse)
import Control.Monad.State
import Control.Monad (void, zipWithM)
import Data.Foldable (traverse_, foldlM, asum, foldrM)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down (Down))
import Data.Bits
import Data.Int
import Data.Tuple (swap)

puzzle24 :: Int -> Solution Result
puzzle24 1 = solve1
puzzle24 2 = solve2

solve1 :: Solution Result
solve1 = Part1 . toInt . uncurry eval . parseInput

solve2 :: Solution Result
solve2 = Part2 . sort . nub . diff (adder 44) . snd . parseInput

data Result = Part1 Int | Part2 [Wire]

instance Show Result where
  show (Part1 x) = show x
  show (Part2 ws) = intercalate "," ws

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

diff :: ConnectionMap -> ConnectionMap -> [Wire]
diff expected actual' = concat $ evalState (mapM (\x -> diffFrom x (debug "" x)) $ endWires expected) actual'
 where
  diffFrom :: Wire -> Wire -> State ConnectionMap [Wire]
  diffFrom ew aw = do
    actual <- get
    let 
      sew = serialWire expected ew
      repl = safeHead $ filter ((== sew) . serialWire actual) $ M.keys actual
    if serialWire expected ew == serialWire actual aw ||
       M.notMember ew expected ||
       M.notMember aw actual
      then pure []
      else case repl of
        Just r -> do
          put $ M.insert aw (actual ! r) $ M.insert r (actual ! aw) actual
          pure $ debug ("swap: " ++ sew ++ "\n") [aw, r]
        Nothing -> case (M.lookup ew expected, M.lookup aw actual) of
          (Just (Connection ea' eg eb'), Just (Connection aa' ag ab'))
            -> do
              let
                  [(sea, ea), (seb, eb)] = sortBy (comparing (length . filter (== '[') . fst)) $ sort $ map (\v -> (serialWire expected v, v)) [ea', eb']
                  [(saa, aa), (sab, ab)] = sortBy (comparing (length . filter (== '[') . fst)) $ sort $ map (\v -> (serialWire actual v, v)) [aa', ab']
              adiff <- if sea == saa then pure [] else diffFrom ea aa
              bdiff <- if seb == sab then pure [] else diffFrom eb ab
              pure $ adiff ++ bdiff
          _ -> error (show (ew, aw))

adder :: Int -> ConnectionMap
adder n = execState (adder' n >>= finish) M.empty
 where
  adder' 0 = halfAdder 0
  adder' k = adder' (k - 1) >>= fullAdder k
  finish w = modify (M.mapKeys (\w' -> if w == w' then wire "z" (n + 1) else w'))

wire :: String -> Int -> Wire
wire v i = v ++ (if i < 10 then "0" ++ show i else show i)

halfAdder :: Int -> State ConnectionMap Wire
halfAdder i = do
  modify (M.insert z (Connection x XOR y))
  modify (M.insert c (Connection x AND y))
  pure c
 where
  x = wire "x" i
  y = wire "y" i
  z = wire "z" i
  c = serialFlat x AND y

fullAdder :: Int -> Wire -> State ConnectionMap Wire
fullAdder i cin = do
  modify (M.insert s'  (Connection x   XOR y))
  modify (M.insert c'  (Connection x   AND y))
  modify (M.insert c'' (Connection cin AND s'))
  modify (M.insert z   (Connection cin XOR s'))
  modify (M.insert c   (Connection c'' OR  c'))
  pure c
 where
  x = wire "x" i
  y = wire "y" i
  s'  = serialFlat x   XOR y
  c'  = serialFlat x   AND y
  c'' = serialFlat cin AND s'
  z = wire "z" i
  c   = serialFlat c'' OR  c'

serialFlat :: Wire -> Gate -> Wire -> String
serialFlat a g b = "[" ++ show g ++ " " ++ min a b ++ " " ++ max a b ++ "]"

serialWire :: ConnectionMap -> Wire -> String
serialWire c w = if M.member w c then serial c (c ! w) else w

serial :: ConnectionMap -> Connection -> String
serial c (Connection a g b) = "[" ++ show g ++ " " ++ min as bs ++ " " ++ max as bs ++ "]"
 where
  as = serialWire c a
  bs = serialWire c b

----------------------------------------

eval :: InputMap -> ConnectionMap -> ValueMap
eval i c = execState (mapM_ evalS (filter endWire $ M.keys c)) M.empty
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
  [a', g, b', "->", c] = words s
  a = min a' b'
  b = max a' b'


