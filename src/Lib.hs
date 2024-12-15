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
    _ -> putStrLn $ "Unknown puzzle #" ++ concat args
 where
  process :: Show a => Solution a -> IO ()
  process solution = do
    input <- fmap lines getContents
    print (solution input)

