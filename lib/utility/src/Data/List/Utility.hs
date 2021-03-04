{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
-- Functions for finding occurrences of elements in a list.
--
-----------------------------------------------------------------------------

module Data.List.Utility where

import           Control.Lens            (Lens', lens)
import           Data.Foldable
import           Data.Key                (Zip(..))
import           Data.List               (sort, sortBy)
import           Data.List.NonEmpty      (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty      as NE
import           Data.Map                (assocs, empty, insertWith)
import           Data.Maybe              (catMaybes)
import           Data.Ord                (comparing)
import           Data.Semigroup.Foldable
import           Data.Set                (insert, intersection)
import           Prelude                 hiding (zipWith)


-- |
-- \( \mathcal{O} \left( n * k \right) \)
--
-- Takes two nested, linear-dimensional structures and transposes their dimensions.
-- It's like performing a matrix transpose operation, but more general.
--
-- ==Example==
--
-- >>> transpose []
-- [[]]
--
-- >>> transpose [[1]]
-- [[1]]
--
-- >>> transpose [ [ 1, 2 ], [ 3, 4 ] ]
-- [[1,3],[2,4]]
--
-- >>> transpose [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9] ]
-- [[1,4,7],[2,5,8],[3,6,9]]
--
-- >>> transpose [ [ 1, 2, 3, 0, 0 ], [ 4, 5, 6, 0 ], [ 7, 8, 9] ]
-- [[1,4,7],[2,5,8],[3,6,9]]
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
    transpose' (e:|[])     =  pure <$> e
    transpose' (e:|(x:xs)) = (cons <$> e) `zap` transpose' (x:|xs)

    cons = (<>) . pure


-- |
-- \( \mathcal{O} \left( n * k \right) \) where \( k \) is the cost to convert the structure to a list in weak head
-- normal form.
--
-- Determines whether a 'Foldable' structure contains a single element.
--
-- ==_Example==
--
-- >>> isSingleton []
-- False
--
-- >>> isSingleton [ () ]
-- True
--
-- >>> isSingleton [ (), () ]
-- False
isSingleton :: Foldable t => t a -> Bool
isSingleton = f . toList
  where
    f [_] = True
    f  _  = False


-- |
-- \( \mathcal{O} \left( n \) where \( n\) is the length of the prefix
--
-- Prepend the foldable structure to the non-empty list.
--
-- ==_Example==
--
-- >>> [] `prepend` xs
-- xs
--
-- >>> [42] `prepend` (1 :| [1,2,3,5])
-- (42 :| [1,1,2,3,5])
prepend :: Foldable f => f a -> NonEmpty a -> NonEmpty a
prepend p ne =
    case toList p of
      []   -> ne
      x:xs -> x :| (xs <> toList ne)


-- |
-- \( \mathcal{O} \left( n \)
--
-- Collect all the 'Just' values of a 'Foldable1' non-empty structure.
--
-- ==== __Examples__
--
-- >>> catMaybes $ Just 1 :| [Nothing, Just 3]
-- Just (1:|[3])
--
-- >>> catMaybes $ Nothing :| [Nothing]
-- Nothing
catMaybes1 :: Foldable1 f => f (Maybe a) -> Maybe (NonEmpty a)
catMaybes1 v =
   let x:|xs = toNonEmpty v
       as    = catMaybes xs
   in  maybe (nonEmpty as) (Just . (:|as)) x


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns the list of elements which are not unique in the input list.
--
-- ==_Example==
--
-- >>> duplicates "duplicate string"
-- "it"
--
-- >>> duplicates "GATACACATCAGATT"
-- "ACGT"
--
-- >>> duplicates [ 'A' .. 'Z' ]
-- []
duplicates :: (Foldable t, Ord a) => t a -> [a]
duplicates = duplicates' . sort . toList
  where
    duplicates' []       = []
    duplicates' [_]      = []
    duplicates' (x:y:ys) = if   x == y
                           then (x:) . duplicates $ dropWhile (==y) ys
                           else duplicates (y:ys)


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns the element that occurs the most often in the list.
--
-- ==_Example==
--
-- >>> mostCommon "GATACACATCAGATT"
-- Just 'A'
--
-- >>> mostCommon "AABCDDDEFGGT"
-- Just 'D'
--
-- >>> mostCommon []
-- Nothing
mostCommon :: (Foldable t, Ord a) => t a -> Maybe a
mostCommon xs
  | null xs   = Nothing
  | otherwise = case occurrences xs of
                  []      -> Nothing
                  (x,_):_ -> Just x


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns a mapping of each unique element in the list paired with how often
-- the element occurs in the list.
--
-- The elements are in descending order of occurrence.
--
-- ==_Example==
--
-- >>> occurrences "GATACACATCAGATT"
-- [('A',6),('T',4),('C',3),('G',2)]
--
-- >>> occurrences "AABCDDDEFGGT"
-- [('D',3),('A',2),('G',2),('B',1),('C',1),('E',1),('F',1),('T',1)]
occurrences :: (Foldable t, Ord a) => t a -> [(a, Word)]
occurrences = collateOccuranceMap . buildOccuranceMap
  where
    buildOccuranceMap = foldr occurrence empty
      where
        occurrence e = insertWith (const succ) e 1
    collateOccuranceMap = sortBy comparator . assocs
      where
        comparator x y = descending $ comparing snd x y
        descending LT = GT
        descending GT = LT
        descending x  = x


-- |
-- \( \mathcal{O} \left( n * \right) \)
--
-- 'chunksOf' is based on Text.chunksOf, but is more general.
--
-- ==_Example==
--
-- >>> chunksOf 3 [ 1 .. 13 ]
-- [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13]]
--
-- >>> chunksOf 5 [ 1 .. 13 ]
-- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13]]
chunksOf :: Foldable t => Int -> t a -> [[a]]
chunksOf n = chunksOf' . toList
  where
    chunksOf' xs =
      case splitAt n xs of
        (y,[]) -> [y]
        (y,ys) -> y : chunksOf' ys


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Useful function to check subsets of lists.
--
-- ==_Example==
--
-- >>> [  5 .. 10 ] `subsetOf` [ 1 .. 13 ]
-- True
--
-- >>> [ 11 .. 15 ] `subsetOf` [ 1 .. 13 ]
-- False
subsetOf :: (Foldable t, Foldable c, Ord a) => t a -> c a -> Bool
subsetOf xs ys = xs' `intersection` ys' == xs'
  where
    xs' = foldr insert mempty xs
    ys' = foldr insert mempty ys


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Applies a transformation to each element of the structure and asserts that
-- transformed values are equal for all elements of the structure.
--
-- See 'invariantTransformation' if you need the equal value returned.
--
-- ==_Example==
--
-- >>> equalityOf (`mod` 10) [ 9, 19, 29, 39, 49 ]
-- True
--
-- >>> equalityOf (`mod`  7) [ 9, 19, 29, 39, 49 ]
-- False
equalityOf :: (Eq b, Foldable t) => (a -> b) -> t a -> Bool
equalityOf f xs =
  case toList xs of
    []   -> True
    [_]  -> True
    y:ys -> let v = f y
            in  all (\e -> v == f e) ys


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Applies a transformation to each element of the structure.
-- If /every/ application of the transformation yields the same result value
-- for each element of the structure then this function will return @Just v@
-- where @v@ is the invariant value across the transformation.
-- If the transformation does not produce an invariant value across the
-- structure, or the structure is empty, this function returns @Nothing@.
--
-- See 'equalityOf' if you want to discard the @Just@ value.
--
-- ==_Example==
--
-- >>> invariantTransformation (`mod` 10) [ 9, 19, 29, 39, 49 ]
-- Just 9
--
-- >>> invariantTransformation (`mod`  7) [ 9, 19, 29, 39, 49 ]
-- Nothing
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
-- \( \mathcal{O} \left( n \right) \)
--
-- Applies a transitive relation over a list and asserts that the relation holds
-- for all pairs of elements. By applying the transitive property, we can assert
-- that the relation hold in liner rather than quadratic time for the collection.
--
-- ==_Example==
--
-- >>> transitivePropertyHolds (\x y -> snd x >= fst y) [ (9,9), (8,7), (6,6), (6,5), (3,4), (3,0) ]
-- True
transitivePropertyHolds :: Foldable f => (a -> a -> Bool) -> f a -> Bool
transitivePropertyHolds p es =
    case toList es of
      []   -> True
      x:xs -> go x xs
  where
    go _    []  = True
    go e   [y]  = p e y
    go e (y:ys) = let !v = p e y
                  in   v && go y ys


-- |
-- \( \mathcal{O} \left( m * n \right) \)
--
-- Provide a pairwise predicate used to filter elements and a nested structure.
-- Returns the "product" of elements across the inner Foldable structure. Each
-- result must have all elements satisfy the predicate when compared to all
-- other elements.
--
-- ==_Example==
--
-- >>> pairwiseSequence (\x y -> snd x /= snd y) [[('A',1),('B',2)],[('X',1),('Y',2),('Z',3)],[('I',1),('J',2),('K',3),('L',4)]]
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
-- \( \mathcal{O} \left( n \right) \)
--
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
-- \( \mathcal{O} \left( n \right) \)
--
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

-- |
-- /O(min(n,m))/
--
-- Perform a simulatanous zipWith and fold where the zip function takes values
-- in some monoid.
foldZipWith
  :: (Monoid m, Foldable f, Foldable t)
  => (a -> b -> m) -> f a -> t b -> m
foldZipWith f fa tb =
  let
    la = toList fa
    lb = toList tb
  in
    fold (zipWith f la lb)

-- |
-- \( \mathcal{O} \left( \mathrm{min} (n_1, n_2, n_3) \right) \)
--
-- Perform a simulatanous zipWith3 and fold where the zip function takes values
-- in some monoid.
foldZipWith3
  :: (Monoid m, Foldable f1, Foldable f2, Foldable f3)
  => (a -> b -> c -> m) -> f1 a -> f2 b -> f3 c -> m
foldZipWith3 f fa fb fc =
  let
    la = toList fa
    lb = toList fb
    lc = toList fc
  in
    fold (zipWith3 f la lb lc)

-- |
-- A class with a 'Control.Lens.Type.Lens' to access the head field of a structure
class HasHead s a | s -> a where
  _head :: Lens' s a

-- |
-- An instance of 'HasHead' for 'NonEmpty'.
instance HasHead (NonEmpty a) a where
  _head = lens NE.head (\ls a' -> a' :| NE.tail ls)

