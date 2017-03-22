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
import Control.Lens
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Range

-- import Debug.Trace


-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: ( DiscreteCharacterMetadata d
                     , RangedCharacterDecoration d  c
                     , DiscreteCharacterMetadata d'
                     , RangedExtensionPostorder  d' c
                     )
                  => d -> [d'] -> d'
additivePostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)


-- |
-- Initializes a leaf node by copying its current value into its preliminary state. Gives it a minimum cost of 0.
--
-- Used on the postorder pass.
initializeLeaf :: ( DiscreteCharacterMetadata d
                  , RangedCharacterDecoration d  c
                  , RangedCharacterDecoration d' c
                  , RangedExtensionPostorder  d' c
                  )
               => d
               -> d'
initializeLeaf curDecoration =
    extendRangedToPostorder curDecoration 0 (toRange label) (unitRange, unitRange) True
  where
    label     = curDecoration ^. intervalCharacter
    unitRange = zeroRange label


-- |
-- Updates the character on the parent of two child nodes to become an 'AdditiveOptimizationDecoration'.
-- Determines the cost by adding the cost of the intersection of the two child nodes, then summing that value
-- with the costs of the two children. The preliminary value of the character is the intersection of the two child intervals.
--
-- Used on the postorder pass.
updatePostOrder :: ( DiscreteCharacterMetadata d
                   , RangedCharacterDecoration d c
                   , DiscreteCharacterMetadata d'
                   , RangedExtensionPostorder  d' c
                   )
                => d -> NonEmpty d' -> d'
updatePostOrder _parentDecoration (x:|[])                     = x
updatePostOrder _parentDecoration (leftChild:|(rightChild:_)) =
    extendRangedToPostorder leftChild totalCost newInterval childIntervals False
  where
    isOverlapping                = lhs `intersects` rhs
    newInterval                  = if isOverlapping
                                   then lhs `intersection`   rhs
                                   else lhs `smallestClosed` rhs
    childIntervals@(lhs, rhs)    = (leftChild ^. preliminaryInterval, rightChild ^. preliminaryInterval)
    totalCost                    = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
    thisNodeCost                 = if isOverlapping
                                   then 0
                                   else upperBound newInterval - lowerBound newInterval


-- |
-- Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: ( Ranged c
                     , Eq (Range (Bound c))
                     , Num (Bound c)
                     , Ord (Bound c)
                     , DiscreteCharacterMetadata d
                     , RangedExtensionPostorder  d  c
--                     , RangedExtensionPostorder  d' c
                     , RangedExtensionPreorder   d' c
                     , RangedCharacterDecoration d' c
                     )
                  => d -> [(Word, d')] -> d'
additivePreOrder childDecoration [] = extendRangedToPreorder childDecoration $ childDecoration ^. preliminaryInterval
additivePreOrder childDecoration ((_, parentDecoration):_)
    | childDecoration ^. isLeaf = finalizeLeaf childDecoration
    | otherwise                 = extendRangedToPreorder childDecoration $ determineFinalState childDecoration parentDecoration


finalizeLeaf :: ( RangedExtensionPreorder   d' c
                , DiscreteCharacterMetadata d
                , RangedPostorderDecoration d  c
                )
             => d -> d'
finalizeLeaf decoration =
    extendRangedToPreorder decoration (decoration ^. preliminaryInterval)
                & intervalCharacter .~ decoration ^. intervalCharacter -- Un-overwrite the character data


-- |
-- Uses the preliminary intervals of a node, its parents, and its children. Follows the three rules of Fitch,
-- modified for additive characters: 1) If the intersection of the current node's character with its parent == the
-- parent interval, use the parent interval; 2) If the union of those two characters == the child, then use the
-- child; 3) Otherwise, find the intersections of the parent and each of the children, union them, then union that
-- with the parent.
--
-- Used on the preorder pass.
determineFinalState :: ( Ranged c
                       , Num (Bound c)
                       , Ord (Bound c)
                       , DiscreteCharacterMetadata    d
                       , RangedExtensionPostorder     d  c
                       , RangedDecorationOptimization d' c
                       )
                    => d -> d'-> Range (Bound c)
determineFinalState childDecoration parentDecoration = resultRange
  where
    curIsSuperset   = (ancestor `intersection` preliminary) == ancestor
    preliminary     = childDecoration  ^. preliminaryInterval
    ancestor        = parentDecoration ^. finalInterval
    (left, right)   = childDecoration  ^. childPrelimIntervals
    chi             = (leftUnionright `union` preliminary) `intersection` ancestor
    leftUnionright  = left `union` right
    resultRange
        | curIsSuperset = ancestor                                                              -- Additive rule 1
        | leftUnionright `intersects` ancestor  =                                               -- Additive rule 2
            if   chi `intersects` preliminary
            then chi
            else largestClosed (closestState preliminary chi) chi
        | otherwise = threeWayRange ancestor preliminary leftUnionright -- Additive rule 3
