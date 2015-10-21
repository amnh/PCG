module Data.List.Utility where

import Data.List     (sort,sortBy)
import Data.Map      (empty,insertWith,toList)
import Data.Ord      (comparing)

duplicates :: Ord a => [a] -> [a]
duplicates = duplicates' . sort
  where 
    duplicates' []       = []
    duplicates' [_]      = []
    duplicates' (x:y:ys) = if   x == y
                           then (x:) . duplicates $ dropWhile (==y) ys 
                           else duplicates (y:ys)

mostCommon :: Ord a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon xs = Just . fst . head $ occurances xs

occurances :: Ord a => [a] -> [(a,Int)]
occurances = collateOccuranceMap . buildOccuranceMap
  where
    buildOccuranceMap = foldr occurance empty 
      where
        occurance e = insertWith (const succ) e 1
    collateOccuranceMap = sortBy comparator . toList
      where
        comparator x y = descending $ comparing snd x y
        descending LT  = GT
        descending GT  = LT
        descending x   = x
