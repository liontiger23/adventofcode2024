module Lib
    ( solve
    ) where

import Text.Read ( readMaybe )
import Util
import Puzzle1
import Puzzle2

solve :: IO ()
solve = do
  str <- getLine
  case map readMaybe $ words str of
    [Just 1, Just p] -> process (puzzle1 p)
    [Just 2, Just p] -> process (puzzle2 p)
    _ -> putStrLn $ "Unknown puzzle #" ++ str
 where
  process :: Show a => Solution a -> IO ()
  process solution = do
    input <- fmap lines getContents
    print (solution input)

