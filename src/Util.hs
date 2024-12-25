module Util
    ( Solution
    , Input
    , safeHead
    , transpose
    , debug
    , debugWith
    ) where

import Debug.Trace (trace)

type Solution a = Input -> a

type Input = [String]

-- >>> take 10 $ transpose [[0..],[0..]]
-- [[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8],[9,9]]
--
-- >>> take 10 $ transpose [[0..1],[0..1]]
-- [[0,0],[1,1]]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

debugWith :: String -> (a -> String) -> a -> a
debugWith s f x = trace (s ++ f x) x
