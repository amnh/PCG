-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Additive.Internal
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
import Control.Lens
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Range
import Numeric.Extended

{-
  TODO: Add these trees to the new test suite

  To test pre-order additive logic Case 1:
    ((A, B), C)
    A: 1
    B: 2
    C: 2

      R
     / \
    A   *
       / \
       B C


  To test pre-order additive logic Case 2a:
    ((A, B), (C, D))
    A: 1
    B: 2
    C: 3
    D: 4

      R
     / \
    *   *
   / \ / \
   A B C D


  To test pre-order additive logic Case 2b:
    (A, (B, ((C, D), (E, F))))
    A: 5
    B: 5
    C: 5
    D: 3
    E: 4
    F: 2

      R
     / \
    A   *
       / \
      B   *
         / \
        *   *
       / \ / \
       C D E F


  To test pre-order additive logic Case 3:
    (A, ((B, C), (D, E)))
    A: 4
    B: 1
    C: 2
    D: 3
    E: 4

      R
     / \
    A   *
       / \
      *   *
     / \ / \
     B C D E


 -}


-- |
-- Used on the post-order (i.e. first) traversal.
-- Applies appropriate logic to internal node and leaf node cases.
additivePostOrder
  :: ( RangedCharacterDecoration d  c
     , RangedExtensionPostorder  d' c
     )
  => d
  -> [d']
  -> d'
additivePostOrder parentDecoration xs =
    case xs of
      []   -> initializeLeaf  parentDecoration -- a leaf
      y:ys -> updatePostOrder parentDecoration $ y:|ys


-- |
-- Initializes a leaf node by copying its current value into its preliminary
-- state. Gives it a minimum cost of 0.
--
-- Used on the post-order pass.
initializeLeaf
  :: ( RangedCharacterDecoration d  c
     , RangedExtensionPostorder  d' c
     )
  => d
  -> d'
initializeLeaf curDecoration = finalDecoration
  where
    finalDecoration = extendRangedToPostorder curDecoration 0 (toRange label) (unitRange, unitRange) True
    label     = curDecoration ^. intervalCharacter
    unitRange = zeroRange label


-- |
-- Updates the character on the parent of two child nodes to become an
-- 'AdditiveOptimizationDecoration'. Determines the cost by adding the cost of
-- the intersection of the two child nodes then summing that value with the
-- costs of the two children. The preliminary value of the character is the
-- intersection of the two child intervals.
--
-- Used on the postorder pass.
updatePostOrder
  :: ( RangedCharacterDecoration d c
     , RangedExtensionPostorder  d' c
     )
  => d
  -> NonEmpty d'
  -> d'
updatePostOrder _parentDecoration (x:|[])                     = x
updatePostOrder _parentDecoration (leftChild:|(rightChild:_)) = finalDecoration
  where
    finalDecoration = extendRangedToPostorder leftChild totalCost newInterval childIntervals False
    totalCost       = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
    isOverlapping   = lhs `intersects` rhs

    newInterval
      | isOverlapping = lhs `intersection`   rhs
      | otherwise     = lhs `smallestClosed` rhs

    thisNodeCost
      | isOverlapping = 0
      | otherwise     = unsafeToFinite $ upperBound newInterval - lowerBound newInterval

    childIntervals@(lhs, rhs) = (leftChild ^. preliminaryInterval, rightChild ^. preliminaryInterval)


-- |
-- Used on the pre-order (i.e. second) traversal.
-- Applies appropriate logic to root node, internal node, and leaf node cases.
additivePreOrder
  :: ( RangedExtensionPostorder  d  c
     , RangedExtensionPreorder   d' c
     )
  => d
  -> [(Word, d')]
  -> d'
additivePreOrder childDecoration [] = extendRangedToPreorder childDecoration $ childDecoration ^. preliminaryInterval
additivePreOrder childDecoration ((_, parentDecoration):_)
  | childDecoration ^. isLeaf = finalizeLeaf childDecoration
  | otherwise                 = extendRangedToPreorder childDecoration $ determineFinalState childDecoration parentDecoration


-- |
-- Finalize a leaf node on a pre-order traversal.
-- Set the preliminary interval as the final interval of the leaf decoration.
finalizeLeaf
  :: ( RangedExtensionPreorder   d' c
     , RangedPostorderDecoration d  c
     )
  => d
  -> d'
finalizeLeaf decoration = finalDecoration
  where
    finalDecoration = extendRangedToPreorder decoration (decoration ^. preliminaryInterval)
                        & intervalCharacter .~ decoration ^. intervalCharacter -- Un-overwrite the character data


-- |
-- Uses the preliminary intervals of a node, its parents, and its children.
-- Follows the three rules of Fitch, modified for additive characters:
--
-- 1. If the intersection of the current node's character with its parent == the
-- parent interval, use the parent interval
--
-- 2. If the union of those two characters == the child, then use the child
--
-- 3. Otherwise, find the intersections of the parent and each of the children,
-- union them, then union that with the parent.
--
-- Used on the pre-order pass.
determineFinalState
  :: ( Ord (Bound c)
     , RangedExtensionPostorder     d  c
     , RangedDecorationOptimization d' c
     ) 
  => d
  -> d'
  -> Range (Bound c)
determineFinalState childDecoration parentDecoration = resultRange
  where
    ancestor       = parentDecoration ^. finalInterval
    preliminary    = childDecoration  ^. preliminaryInterval
    (left, right)  = childDecoration  ^. childPrelimIntervals
    chi            = (leftUnionRight `union` preliminary) `intersection` ancestor
    leftUnionRight = left `union` right
    resultRange
      -- Additive rule 1
      | (ancestor `intersection` preliminary) == ancestor = ancestor

      -- Additive rule 2
      | leftUnionRight `intersects` ancestor =
          if   chi `intersects` preliminary
          then chi
          else chi `largestClosed` (preliminary `closestStateTo` chi)

      -- Additive rule 3
      | otherwise = threeWayRange ancestor preliminary leftUnionRight
