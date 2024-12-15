{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module DisjointSet
    ( DisjointSet
    , empty
    , insert
    , append
    , fromList
    , fromListBy
    , elems
    , representatives
    , union
    , unionS
    , find
    , findS
    , equivalent
    , equivalenceClasses
    , equivalenceClass
    ) where

import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List (groupBy)
import Control.Monad.State
import Data.Foldable (traverse_)

newtype DisjointSet a = DisjointSet
  { internals :: Map a a }
  deriving (Show)

instance Ord a => Monoid (DisjointSet a) where
  mappend = (<>)
  mempty = empty

instance Ord a => Semigroup (DisjointSet a) where
  (<>) = append

-- >>> union 2 3 $ union 1 2 $ fromList [1,2,3]
-- DisjointSet {internals = fromList [(1,1),(2,1),(3,1)]}
--
-- >>> equivalenceClasses $ union 1 2 $ fromList [1,2,3]
-- [[1,2],[3]]
--
-- >>> equivalenceClasses $ execState (unionS 1 2) $ fromList [1,2,3]
-- [[1,2],[3]]

empty :: DisjointSet a
empty = DisjointSet M.empty

insert :: Ord a => a -> DisjointSet a -> DisjointSet a
insert x ds@(DisjointSet es)
  | M.member x es = ds
  | otherwise = DisjointSet (M.insert x x es)

append :: Ord a => DisjointSet a -> DisjointSet a -> DisjointSet a
append xs ys = foldr (uncurry union) xs $ M.toList $ internals ys

fromList :: Ord a => [a] -> DisjointSet a
fromList = foldr insert empty

-- >>> equivalenceClasses $ fromListBy (\x y -> x == y + 1 || y == x + 1) [1,2,4,5,6]
-- [[1,2],[4,5,6]]
-- >>> equivalenceClasses $ fromListBy (\x y -> x == y + 1 || y == x + 1) [5,1,4,2,6]
-- [[1,2],[4,5,6]]
-- >>> groupBy (\x y -> x == y + 1 || y == x + 1) [1,2,4,5,6]
-- [[1,2],[4,5],[6]]
-- >>> groupBy (\x y -> x == y + 1 || y == x + 1) [5,1,4,2,6]
-- [[5],[1],[4],[2],[6]]

fromListBy :: Ord a => (a -> a -> Bool) -> [a] -> DisjointSet a
fromListBy eq xs = execState collect $ fromList xs
 where
  collect = traverse_ (uncurry unionS) $ filter (uncurry eq) $ pairs xs --[(x, y) | x <- xs, y <- xs, x `eq` y]
  pairs [] = []
  pairs (y:ys) = map (y,) ys ++ pairs ys


elems :: DisjointSet a -> [a]
elems = M.keys . internals

representatives :: Ord a => DisjointSet a -> [a]
representatives (DisjointSet m) = M.keys $ M.filterWithKey (==) m

union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y ds = DisjointSet $ M.insert yroot xroot $ internals ds'
 where
  ds' = insert x $ insert y ds
  xroot = fromJust (find x ds')
  yroot = fromJust (find y ds')

unionS :: Ord a => a -> a -> State (DisjointSet a) ()
unionS x y = modify (insert x . insert y) *> do
  xroot <- fromJust <$> findS x
  yroot <- fromJust <$> findS y
  modify (DisjointSet . M.insert yroot xroot . internals)

find :: Ord a => a -> DisjointSet a -> Maybe a
find x ds@(DisjointSet es) = case M.lookup x es of
  Nothing -> Nothing
  Just parent
    | parent == x -> Just x
    | otherwise   -> find parent ds

findS :: Ord a => a -> State (DisjointSet a) (Maybe a)
findS x = gets (M.lookup x . internals) >>= \case
  Nothing -> pure Nothing
  Just parent
    | parent == x -> pure $ Just x
    | otherwise   -> do
      r <- findS parent
      modify (DisjointSet . M.insert x (fromJust r) . internals)
      pure r

equivalent :: Ord a => DisjointSet a -> a -> a -> Bool
equivalent ds x y = isJust rootx && isJust rooty && rootx == rooty
 where
  rootx = find x ds
  rooty = find y ds

equivalenceClasses :: Ord a => DisjointSet a -> [[a]]
equivalenceClasses ds = map (equivalenceClass ds) $ representatives ds

equivalenceClass :: Ord a => DisjointSet a -> a -> [a]
equivalenceClass ds x = filter (\y -> r == find y ds) $ elems ds
 where
  r = find x ds
