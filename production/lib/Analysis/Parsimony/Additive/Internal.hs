-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Additive
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard Additive character analysis (cost and medians).
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-- Note that this is the same procedure as for continuous characters, but with
-- costs of Word, not Double.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Additive.Internal where

import Bio.Character.Decoration.Additive
--import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
import Control.Lens
-- import Control.Monad  (join)
-- import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Range
-- import Data.Word
-- import Debug.Trace


-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: ( DiscreteCharacterDecoration d c
                     , Ranged c
                     , Bounded (Bound c)
                     , Num (Bound c)
                     , Ord (Bound c)
                     )
                  => d
                  -> [AdditiveOptimizationDecoration c]
                  -> AdditiveOptimizationDecoration c
additivePostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration         -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)


-- | Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: ( Ranged c
                     , Eq (Range (Bound c))
                     , Bounded (Bound c)
                     , Num (Bound c)
                     , Ord (Bound c)
--                     , AdditiveDecoration d c
                     )
                  => AdditiveOptimizationDecoration c
                  -> [(Word, AdditiveOptimizationDecoration c)]
                  -> AdditiveOptimizationDecoration c
additivePreOrder childDecoration []                      = finalDecoration                      -- root
    where
        interDecoration = computeFinalDiscrete (childDecoration ^. preliminaryInterval) childDecoration
        finalDecoration = interDecoration & finalInterval .~ childDecoration ^. preliminaryInterval

additivePreOrder childDecoration ((_, parentDecoration):_)
    | childDecoration ^. isLeaf = childDecoration & finalInterval .~ childDecoration ^. preliminaryInterval
    | otherwise                 = determineFinalState childDecoration parentDecoration


-- |
-- Updates the character on the parent of two child nodes to become an 'AdditiveOptimizationDecoration'.
-- Determines the cost by adding the cost of the intersection of the two child nodes, then summing that value
-- with the costs of the two children. The preliminary value of the character is the intersection of the two child intervals.
--
-- Used on the postorder pass.
updatePostOrder :: ( DiscreteCharacterDecoration d c
                   , Ranged c
                   , Bounded (Bound c)
                   , Num (Bound c)
                   , Ord (Bound c)
                   )
                => d
                -> NonEmpty (AdditiveOptimizationDecoration c)
                -> AdditiveOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                     = x                     -- Shouldn't be possible, but here for completion.
updatePostOrder _parentDecoration (leftChild:|(rightChild:_)) = {- trace (show newMin ++ " " ++ show newMax ++ " " ++ show totalCost) $ -}
    extendDiscreteToAdditive leftChild totalCost newInterval (unitRange minBound) childIntervals False
  where
    newInterval                  = if isOverlapping
                                   then lhs `intersection`   rhs
                                   else lhs `smallestClosed` rhs
    childIntervals@(lhs, rhs)    = (leftChild ^. preliminaryInterval, rightChild ^. preliminaryInterval)
    totalCost                    = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
    thisNodeCost                 = if isOverlapping
                                   then 0
                                   else upperBound newInterval - lowerBound newInterval
    isOverlapping                = lhs `intersects` rhs



-- | Initializes a leaf node by copying its current value into its preliminary state. Gives it a minimum cost of 0.
--
-- Used on the postorder pass.
initializeLeaf :: ( DiscreteCharacterDecoration d c
                  , Ranged c
                  , Bounded (Bound c)
                  , Num (Bound c)
                  , Ord (Bound c)
                  )
               => d
               -> AdditiveOptimizationDecoration c
initializeLeaf curDecoration =
    extendDiscreteToAdditive curDecoration 0 (toRange label) zeroRange (zeroRange, zeroRange) True
    where
        label     = curDecoration ^. discreteCharacter
        zeroRange = unitRange minBound


-- | Uses the preliminary intervals of a node, its parents, and its children. Follows the three rules of Fitch,
-- modified for additive characters: 1) If the intersection of the current node's character with its parent == the
-- parent interval, use the parent interval; 2) If the union of those two characters == the child, then use the
-- child; 3) Otherwise, find the intersections of the parent and each of the children, union them, then union that
-- with the parent.
--
-- Used on the preorder pass.
determineFinalState :: ( Ranged c
                       , Eq (Range (Bound c))
                       , Bounded (Bound c)
                       , Num (Bound c)
                       , Ord (Bound c)
                       )
                    => AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
determineFinalState childDecoration parentDecoration = childDecoration & discreteCharacter .~ finalCharacter
  where
    finalCharacter  = fromRange x (childDecoration ^. discreteCharacter)
    curIsSuperset   = (ancestor `intersection` preliminary) == ancestor
    preliminary     = childDecoration  ^. preliminaryInterval
    ancestor        = parentDecoration ^. finalInterval
    (left, right)   = childDecoration  ^. childPrelimIntervals
    chi             = (leftUnionright `union` preliminary) `intersection` ancestor
    leftUnionright  = left `union` right
    prelimClosestA  = closestState preliminary ancestor
    childsCloseestA = closestState leftUnionright ancestor
    x
        | curIsSuperset = ancestor                                                              -- Additive rule 1
        | leftUnionright `intersects` ancestor  =                                               -- Additive rule 2
            if   chi `intersects` preliminary
            then chi
            else largestClosed (closestState preliminary chi) chi
        | otherwise = fromTuple (min prelimClosestA childsCloseestA, max prelimClosestA childsCloseestA)  -- Additive rule 3


computeFinalDiscrete :: ( Ranged c
                        , HasDiscreteCharacter d c
                        , HasFinalInterval     d (Range (Bound c))
                        )
                     => Range (Bound c) -> d -> d
computeFinalDiscrete interval decoration =
    decoration
      & finalInterval     .~ interval
      & discreteCharacter .~ (fromRange interval character)
  where
    character = decoration ^. discreteCharacter


{-
-- | True if there is any overlap between the two intervals
intersects :: (Word, Word) -> (Word, Word) -> Bool
intersects leftChild rightChild =
    (rightSmallest <= leftLargest) && (rightLargest >= leftSmallest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild


-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible. Does
-- not assume there's an overlap.
intersection :: (Word, Word) -> (Word, Word) -> (Word, Word)
intersection leftChild rightChild = (max leftSmallest rightSmallest, min leftLargest rightLargest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild


-- | The smallest closed interval is the smallext interval between two non-overlapping intervals, so the
-- largest value in the leftmost interval on the number line to the smallest value of the rightmost.
smallestClosed :: (Word, Word) -> (Word, Word) -> (Word, Word)
smallestClosed leftChild rightChild = (min leftLargest rightLargest, max leftSmallest rightSmallest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild


-- | The largest closed interval between the single value on the left and the interval on the right.
-- This is the equivalent of `union`, but works for a single value on the left, rather than an interval.
largestClosed :: Word -> (Word, Word) -> (Word, Word)
largestClosed lhs rhs = (min lhs rightLargest, max lhs rightSmallest)
    where
        (rightSmallest, rightLargest) = rhs


-- | The closest state is the closest value in the left interval to the right interval.
-- This assumes that there is no overlap between the intervals. If the two intervals intersect
-- incorrect results will be returned.
closestState :: (Word, Word) -> (Word, Word) -> Word
closestState (leftSmallest, leftLargest) (rightSmallest, _rightLargest)
    | leftLargest < rightSmallest = leftLargest
    | otherwise                   = leftSmallest


-- |
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest possible
-- value to the largest possible, considering the values in both intervals.
--
-- Works for overlapped or subsetted intervals, as well as non-overlapping intervals
union :: (Word, Word) -> (Word, Word)-> (Word, Word)
union leftChild rightChild = (min leftSmallest rightSmallest, max leftLargest rightLargest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild

-}
