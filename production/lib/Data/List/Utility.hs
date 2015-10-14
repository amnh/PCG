module Data.List.Utility where

import Data.List (sort)

duplicates :: Ord a => [a] -> [a]
duplicates = duplicates' . sort
  where 
    duplicates' []       = []
    duplicates' [_]      = []
    duplicates' (x:y:ys) = if   x == y
                           then (x:) . duplicates $ dropWhile (==y) ys 
                           else duplicates (y:ys)

