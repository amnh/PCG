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

import Data.Foldable
import Data.Key           (Zip(..))
import Data.List          (sort, sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map           (assocs, empty, insertWith)
import Data.Semigroup
import Data.Ord           (comparing)
import Data.Set           (insert, intersection)


-- |
-- Takes two nested, linear-dimentional structures and transposes thier dimensions.
-- It's like performing a matrix transpose operation, but more general.
transpose
  :: ( Applicative f
     , Applicative t
     , Semigroup (t a)
     , Traversable t
     , Zip f
     )
  => t (f a) -> f (t a)
transpose value =
    case toList value of
      []   -> sequenceA value
      x:xs -> transpose' $ x:|xs
  where
    transpose' (e:|[])     = (pure <$> e)
    transpose' (e:|(x:xs)) = (cons <$> e) `zap` (transpose' (x:|xs))

    cons = (<>) . pure


-- |
-- Determines whether a foldable structure contains a single element.
isSingleton :: Foldable t => t a -> Bool
isSingleton = f . toList
  where
    f [_] = True
    f  _  = False


-- |
-- Returns the list of elements which are not unique in the input list.
duplicates :: (Foldable t, Ord a) => t a -> [a]
duplicates = duplicates' . sort . toList
  where 
    duplicates' []       = []
    duplicates' [_]      = []
    duplicates' (x:y:ys) = if   x == y
                           then (x:) . duplicates $ dropWhile (==y) ys 
                           else duplicates (y:ys)


-- |
-- Returns the element that occurs the most often in the list.
mostCommon :: (Foldable t, Ord a) => t a -> Maybe a
mostCommon xs
  | null xs   = Nothing
  | otherwise = case occurances xs of
                  []      -> Nothing
                  (x,_):_ -> Just x


-- |
-- Returns a mapping of each unique element in the list paired with how often
-- the element occurs in the list.
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


-- |
-- chunksOf is based on Text.chunksOf, but is more general.
chunksOf :: Foldable t => Int -> t a -> [[a]]
chunksOf n = chunksOf' . toList
  where
    chunksOf' xs =
      case splitAt n xs of
        (y,[]) -> [y]
        (y,ys) -> y : chunksOf' ys


-- |
-- Useful function to check subsets of lists.
subsetOf :: (Foldable t, Foldable c, Ord a) => t a -> c a -> Bool
subsetOf xs ys = xs' `intersection` ys' == xs'
  where
    xs' = foldr insert mempty xs 
    ys' = foldr insert mempty ys 


-- |
-- Applies a transformation to each element fo the structure and asserts that
-- transformed values are equal for all elements of the structure.
equalityOf :: (Eq b, Foldable t) => (a -> b) -> t a -> Bool
equalityOf f xs =
  case toList xs of
    []   -> True
    [_]  -> True
    y:ys -> all (\e -> f y == f e) ys


-- |
-- Applies a transformation to each element fo the structure.
-- If /every/ application of the transformation yeilds the same result value
-- for each element of the structure then this function will return @Just v@
-- where @v@ is the invariant value accross the transformation.
-- If the transformation does not produce an invariant value accross the
-- structure, or the structure is empty, this function returns @Nothing@.
invariantTransformation :: (Eq b, Foldable t) => (a -> b) -> t a -> Maybe b
invariantTransformation f xs =
  case toList xs of
    []   -> Nothing
    y:ys ->
      let v = f y
      in  if all (\e -> f e == v) ys
          then Just v
          else Nothing


-- |
-- Provide a pairwise predicate used to filter elements and a nested structure.
-- Returns the "product" of elements across the inner Foldable structure. Each
-- result must have all elements satisfy the predicate when compared to all
-- other elements.
--
-- ==_Example==
--
-- >>> pairwiseSequence (\x y = snd x /= snd y) [[('A',1),('B',2)],[('X',1),('Y',2),('Z',3)],[('I',1),('J',2),('K',3),('L',4)]]
-- [[('A',1),('Y',2),('K',3)],[('A',1),('Y',2),('L',4)],[('A',1),('Z',3),('J',2)],[('A',1),('Z',3),('L',4)],[('B',2),('X',1),('K',3)],[('B',2),('X',1),('L',4)],[('B',2),('Z',3),('I',1)],[('B',2),('Z',3),('L',4)]]
--
pairwiseSequence :: (Foldable t, Foldable t') => (a -> a -> Bool) -> t (t' a) -> [[a]]
pairwiseSequence predicate structure = f [] $ toList <$> toList structure
  where
    f thread     [] = [reverse thread]
    f thread (x:xs) = foldMap g x
      where
        g e = f (e:thread) $ fmap (filter (predicate e)) xs


-- |
-- The largest elements of a possibly empty structure with respect to the given
-- comparison function. If multiple elements are equal under the comparison
-- function, then all the equally maximal elements are returned in the order
-- they existed in the foldable structure.
--
-- The empty list is returned if and only if an empty structure was provided.
--
-- Similar to 'Data.Foldable.maximumBy'. For non-empty structures, the following
-- will hold:
--
-- > head . maximaBy == maximumBy
maximaBy :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
maximaBy cmp = foldr f []
  where
    f e es =
        case es of
          []  -> [e]
          x:_ ->
              case cmp e x of
                EQ -> e:es
                GT -> [e]
                LT -> es


-- |
-- The smallest elements of a possibly empty structure with respect to the given
-- comparison function. If multiple elements are equal under the comparison
-- function, then all the equally minimal elements are returned in the order
-- they existed in the foldable structure.
--
-- The empty list is returned if and only if an empty structure was provided.
--
-- Similar to 'Data.Foldable.minimumBy'. For non-empty structures, the following
-- will hold:
--
-- > head . minimaBy == minimumBy
minimaBy :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
minimaBy cmp = foldr f []
  where
    f e es =
        case es of
          []  -> [e]
          x:_ ->
              case cmp e x of
                EQ -> e:es
                GT -> es
                LT -> [e]
