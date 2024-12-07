module Util
    ( Solution
    , safeHead
    ) where

type Solution a = [String] -> a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
