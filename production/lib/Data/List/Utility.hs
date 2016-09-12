-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for finding occurances of elements in a list.
--
----------------------------------------------------------------------------- 

module Data.List.Utility where

import Data.Set      (insert, intersection)
import Data.Foldable
import Data.List     (sort, sortBy)
import Data.Map      (assocs, empty, insertWith)
import Data.Ord      (comparing)

-- | Determines whether a foldable structure contains a single element.
isSingleton :: Foldable t => t a -> Bool
isSingleton = f . toList
  where
    f [_] = True
    f  _  = False

-- | Returns the list of elements which are not unique in the input list.
duplicates :: (Foldable t, Ord a) => t a -> [a]
duplicates = duplicates' . sort . toList
  where 
    duplicates' []       = []
    duplicates' [_]      = []
    duplicates' (x:y:ys) = if   x == y
                           then (x:) . duplicates $ dropWhile (==y) ys 
                           else duplicates (y:ys)

-- | Returns the element that occurs the most often in the list.
mostCommon :: (Foldable t, Ord a) => t a -> Maybe a
mostCommon xs
  | null xs   = Nothing
  | otherwise = case occurances xs of
                  []      -> Nothing
                  (x,_):_ -> Just x

-- | Returns a mapping of each unique element in the list
--   paired with how often the element occurs in the list.
occurances :: (Foldable t, Ord a) => t a -> [(a,Int)]
occurances = collateOccuranceMap . buildOccuranceMap
  where
    buildOccuranceMap = foldr occurance empty 
      where
        occurance e = insertWith (const succ) e 1
    collateOccuranceMap = sortBy comparator . assocs
      where
        comparator x y = descending $ comparing snd x y
        descending LT  = GT
        descending GT  = LT
        descending x   = x

-- | chunksOf is based on Text.chunksOf, but is more general.
chunksOf :: Foldable t => Int -> t a -> [[a]]
chunksOf n = chunksOf' . toList
  where
    chunksOf' xs =
      case splitAt n xs of
        (y,[]) -> [y]
        (y,ys) -> y : chunksOf' ys

-- | Useful function to check subsets of lists.
subsetOf :: (Foldable t, Foldable c, Ord a) => t a -> c a -> Bool
subsetOf xs ys = xs' `intersection` ys' == xs'
  where
    xs' = foldr insert mempty xs 
    ys' = foldr insert mempty ys 

-- | Applies a transformation to each element fo the structure and asserts that
--   transformed values are equal for all elements of the structure.
equalityOf :: (Eq b, Foldable t) => (a -> b) -> t a -> Bool
equalityOf f xs =
  case toList xs of
    []   -> True
    [_]  -> True
    y:ys -> all (\e -> f y == f e) ys
