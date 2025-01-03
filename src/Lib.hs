module Lib
    ( solve
    ) where

import Text.Read ( readMaybe )
import Util
import Puzzle1
import Puzzle2
import Puzzle3
import Puzzle4
import Puzzle5
import Puzzle6
import Puzzle7
import Puzzle8
import Puzzle9
import Puzzle10
import Puzzle11
import Puzzle12
import Puzzle13
import Puzzle14
import Puzzle15
import Puzzle16
import Puzzle17
import Puzzle18
import Puzzle19
import Puzzle20
import Puzzle21
import Puzzle22
import Puzzle23
import Puzzle24
import Puzzle25
import System.Environment (getArgs)

solve :: IO ()
solve = do
  args <- getArgs
  case map readMaybe args of
    [Just 1, Just p] -> process (puzzle1 p)
    [Just 2, Just p] -> process (puzzle2 p)
    [Just 3, Just p] -> process (puzzle3 p)
    [Just 4, Just p] -> process (puzzle4 p)
    [Just 5, Just p] -> process (puzzle5 p)
    [Just 6, Just p] -> process (puzzle6 p)
    [Just 7, Just p] -> process (puzzle7 p)
    [Just 8, Just p] -> process (puzzle8 p)
    [Just 9, Just p] -> process (puzzle9 p)
    [Just 10, Just p] -> process (puzzle10 p)
    [Just 11, Just p] -> process (puzzle11 p)
    [Just 12, Just p] -> process (puzzle12 p)
    [Just 13, Just p] -> process (puzzle13 p)
    [Just 14, Just p] -> process (puzzle14 p)
    [Just 15, Just p] -> process (puzzle15 p)
    [Just 16, Just p] -> process (puzzle16 p)
    [Just 17, Just p] -> process (puzzle17 p)
    [Just 18, Just p] -> process (puzzle18 p)
    [Just 19, Just p] -> process (puzzle19 p)
    [Just 20, Just p] -> process (puzzle20 p)
    [Just 21, Just p] -> process (puzzle21 p)
    [Just 22, Just p] -> process (puzzle22 p)
    [Just 23, Just p] -> process (puzzle23 p)
    [Just 24, Just p] -> process (puzzle24 p)
    [Just 25, Just p] -> process (puzzle25 p)
    _ -> putStrLn $ "Unknown puzzle #" ++ concat args
 where
  process :: Show a => Solution a -> IO ()
  process solution = do
    input <- fmap lines getContents
    print (solution input)

