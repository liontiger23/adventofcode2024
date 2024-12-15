module Util
    ( Solution
    , safeHead
    , debug
    ) where

import Debug.Trace (trace)

type Solution a = [String] -> a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

debug :: Show a => String ->  a -> a
debug s x = trace (s ++ show x) x
