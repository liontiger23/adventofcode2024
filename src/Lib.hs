module Lib
    ( solve
    ) where

import Text.Read ( readMaybe )
import Util
import Puzzle1
import Puzzle2
import Puzzle3
import Puzzle4

solve :: IO ()
solve = do
  str <- getLine
  case map readMaybe $ words str of
    [Just 1, Just p] -> process (puzzle1 p)
    [Just 2, Just p] -> process (puzzle2 p)
    [Just 3, Just p] -> process (puzzle3 p)
    [Just 4, Just p] -> process (puzzle4 p)
    _ -> putStrLn $ "Unknown puzzle #" ++ str
 where
  process :: Show a => Solution a -> IO ()
  process solution = do
    input <- fmap lines getContents
    print (solution input)

