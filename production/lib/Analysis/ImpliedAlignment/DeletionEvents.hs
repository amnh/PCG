-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.DeletionEvents
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Analysis.ImpliedAlignment.DeletionEvents where

import           Data.IntSet          (IntSet)
import qualified Data.IntSet    as IS
import           Data.List            (intercalate)
import           Data.Monoid
import           Data.MonoTraversable

-- TODO: Use BitVectors here for efficency!
newtype DeletionEvents = DE IntSet deriving (Eq)

instance Monoid DeletionEvents where
  mempty = DE mempty

  {- | /O(m)/ where m is sequence length

       When we have two Deletion Event collections and we want to merge them
       into a new, larger deletion event collection, we must take into account
       that one collection is of anscestoral events and the other of descendant
       events. There will likely be a shift in the indices' "frames of reference"
       which will require incrementation of the descendant deletion event
       collection.


       |> CASE 1 (simple)
       -=-=-=-=-=-=-=-=-

       Consider the comparison between the follwoing sequences:

       Anscestor:  GATTACA
       Descendant: GAACA
       Alignment:  GA--ACA
       Deletion Events: [2,3]

       Consider the comparison betwen the folowing sequences:

       Anscestor:  GAACA
       Descendant: GAAC
       Alignment:  GAAC-
       Deletion Events: [4]

       We must consider the total alignment history when merging the two deletion
       event collections so that the deletion events of the child have a reference
       frame to the root sequence

       Alignment History:
         Grandparent:  GATTACA
         Parent:       GA--ACA
         Child:        GA--AC-

       Deletion event collections:
          [2,3] <> [4] = [2,3,6]

       Grandparent:  GATTACA
       Child:        GA--AC-

       Note that the index of 4 on the righthand side is incremented by 2 to 6.
       This is because there are 2 indicies in the ancestor deletion event
       collection that are less than 4.


       |> CASE 2: (complex)
       -=-=-=-=-=-=-=-=-

       Consider the comparison between the follwoing sequences:

       Anscestor:  GATTACATA
       Descendant: GACATA
       Alignment:  GA---CATA
       Deletion Events: [2,3,4]

       Consider the comparison betwen the folowing sequences:

       Anscestor:  GACATA
       Descendant: GAAA
       Alignment:  GA-A-A
       Deletion Events: [2,4]

       When the descendant deletion event collection has a deletion event with
       an index that is a member of the acestor deletion event collection, the
       descendant index must be updated by the number of sequential elements in
       the ancestor deletion collection starting from the matching index.

       Alignment History:
         Grandparent:  GATTACATA
         Parent:       GA--ACATA
         Child:        GA----A-A

       Deletion event collections:
          [2,3,4] <> [2,4] = [2,3,4,5,7]

       Grandparent:  GATTACA
       Child:        GA--A-A

       Note that the index of 2 on the righthand side is incremented by 3 to 5.
       This is because there are 3 *consecutive* indicies in the ancestor
       deletion event collection starting at index 2.

       Note that the index of 4 on the righthand side is incremented by 3 to 7.
       This is because there are 2 indicies in the ancestor deletion event
       collection that are less than 4 *and* there is 1 *consecutive* index in
       the ancestor deletion event collection starting at index 4.

  -}
  
  ancestorSet@(DE as) `mappend` descendantSet = DE . (as <>) $ ancestorSet `incrementDescendant` descendantSet

incrementDescendant :: DeletionEvents -> DeletionEvents -> IntSet
incrementDescendant (DE ancestorSet) (DE descendantSet) = incrementedDescendantSet
  where
      (_,_,incrementedDescendantSet) = ofoldl' f (0, otoList ancestorSet, mempty) descendantSet
      f (counter, [], is) descendantIndex = (counter, [], (counter + descendantIndex) `IS.insert` is)
      f (counter, as, is) descendantIndex =
        case remaining of
           []   -> (counter', [], (counter' + descendantIndex) `IS.insert` is)
           x:xs ->
             if   x > descendantIndex
             then (counter'    , x:xs, (      counter' + descendantIndex) `IS.insert` is)
             else (counter' + 1,   xs, (inc + counter' + descendantIndex) `IS.insert` is)
        where
          (prev, remaining) = span (< descendantIndex) as
          counter' = length prev + counter
          inc = consecutiveLength remaining

--          descendantIndex' = descendantIndex + counter
--          incrementation   = consecutiveLength . drop (descendantIndex' - 1) $ otoList ancestorSet

          consecutiveLength :: (Eq a, Num a) => [a] -> Int
          consecutiveLength = g 0
            where
              g n       [] = n
              g n      [_] = n + 1
              g n (x:y:ys)
                | x+1 == y  = g (n+1) (y:ys)
                | otherwise = n + 1
 
instance Show DeletionEvents where
  show = (\x -> "{" <> x <> "}") . intercalate "," . fmap show . otoList

-- | The element for monomorphic maps & folds.
type instance Element DeletionEvents = Int

-- | Performs monomporphic fold over the 'DeletionEvents'.
instance MonoFoldable DeletionEvents where

  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  {-# INLINE ofoldMap #-}
  ofoldMap f (DE de) = ofoldMap f de

  -- | Right-associative fold of a monomorphic container.
  {-# INLINE ofoldr #-}
  ofoldr f e (DE de) = ofoldr f e de

  -- | Strict left-associative fold of a monomorphic container.
  {-# INLINE ofoldl' #-}
  ofoldl' f e (DE de) = ofoldl' f e de

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldr1Ex #-}
  ofoldr1Ex f (DE de) = ofoldr1Ex f de

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldl1Ex' #-}
  ofoldl1Ex' f (DE de) = ofoldl1Ex' f de

  {-# INLINE onull #-}
  onull (DE de) = onull de

  {-# INLINE olength #-}
  olength (DE de) = olength de

fromList :: (Enum (Element t), MonoFoldable t) => t -> DeletionEvents
fromList = DE . ofoldMap (IS.singleton . fromEnum)
