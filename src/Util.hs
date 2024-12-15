module Util
    ( Solution
    , safeHead
    , debug
    , debugWith
    ) where

import Debug.Trace (trace)

type Solution a = [String] -> a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

debugWith :: String -> (a -> String) -> a -> a
debugWith s f x = trace (s ++ f x) x
