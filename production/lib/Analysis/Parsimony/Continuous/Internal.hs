-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard Continuous character analysis (cost and medians).
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-- Note that this is the same procedure as for additive characters, but with
-- costs of Double, not Word.
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Continuous.Internal where

import Bio.Character.Decoration.Continuous
--import Bio.Character.Decoration.Discrete
--import Bio.Character.Encodable
import Control.Lens
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty( (:|) ))
--import Data.Double


-- data ContinuousOptimizationDecoration c = ContinuousOptimizationDecoration
--     { characterCost           :: Double      -- cost of the subtree
--     , preliminaryInterval :: c           -- held here until final state is determined and we can assign that into discreteCharacter
--     , isLeaf            :: Bool
--     , interval          :: c
--     , childPrelims      :: (c, c)
--     }

-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: (DiscreteCharacterDecoration d c, FiniteBits c)
                  => d
                  -> [ContinuousOptimizationDecoration c]
                  -> ContinuousOptimizationDecoration c
additivePostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration         -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)

-- | Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: ContinuousOptimizationDecoration c
                  -> [(Double, ContinuousOptimizationDecoration c)]
                  -> ContinuousOptimizationDecoration c
additivePreOrder childDecoration (_:_:_)                 = childDecoration   -- multiple parents; shouldn't be possible,
                                                                                --    but here for completion
additivePreOrder childDecoration []                      = childDecoration   -- is a root TODO: need to change preliminary
                                                                                --    to final
additivePreOrder childDecoration [(_, parentDecoration)] =
    if childDecoration ^. isLeaf
        then childDecoration
        else determineFinalState childDecoration parentDecoration

-- |
-- Updates the character on the parent of two child nodes to become a 'ContinuousOptimizationDecoration'.
-- Determines the cost by adding the cost of the intersection of the two child nodes, then summing that value
-- with the costs of the two children. The preliminary value of the character is the intersection of the two child intervals.
--
-- Used on the postorder pass.
updatePostOrder :: DiscreteCharacterDecoration d c
                => d
                -> NonEmpty (ContinuousOptimizationDecoration c)
                -> ContinuousOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                    = x                     -- Shouldn't be possible,
                                                                                     --    but here for completion.
updatePostOrder  parentDecoration (leftChild:|(rightChild:_)) = returnNodeDecoration  -- Not a leaf.
    where
        (newMin, newMax)              = leftInterval `intersect` rightInterval
        (leftInterval, rightInterval) = join bimap (^. preliminaryInterval) (leftChild, rightChild)
        newInterval                   = (newMin, newMax)
        totalCost                     = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
        thisNodeCost                  = newMax - newMin
        returnNodeDecoration          =
            extendDiscreteToContinuous parentDecoration totalCost newInterval (leftInterval, rightInterval) False

-- | Initializes a leaf node by copying its current value into its preliminary state. Gives it a minimum cost of 0.
--
-- Used on the postorder pass.
initializeLeaf :: (DiscreteCharacterDecoration d c, FiniteBits c)
               => d
               -> ContinuousOptimizationDecoration c
initializeLeaf curDecoration =
    extendDiscreteToContinuous curDecoration zero (lower, higher) ((zero,zero),(zero,zero)) True
    where
        label  = curDecoration ^. discreteCharacter
        lower  = label
        higher = label
        zero   = 0.0

-- | Uses the preliminary intervals of a node, its parents, and its children. Follows the three rules of Fitch,
-- modified for continuous characters: 1) If the intersection of the current node's character with its parent == the
-- parent interval, use the parent interval; 2) If the union of those two characters == the child, then use the
-- child; 3) Otherwise, find the intersections of the parent and each of the children, union them, then union that
-- with the parent.
--
-- Used on the preorder pass.
determineFinalState :: ContinuousOptimizationDecoration c
                    -> ContinuousOptimizationDecoration c
                    -> ContinuousOptimizationDecoration c
determineFinalState childDecoration parentDecoration = finalDecoration
    where
        finalDecoration = childDecoration  &  preliminaryInterval .~ (thisMin, thisMax)
        preliminary     = childDecoration  ^. preliminaryInterval
        ancestor        = parentDecoration ^. preliminaryInterval
        (left, right)   = childDecoration  ^. childPrelimIntervals
        thirdCase       = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor)
        (thisMin, thisMax)
            | (ancestor `intersect` preliminary) == ancestor     = ancestor                        -- Continuous rule 1
            | (ancestor `union`     preliminary) == preliminary  = preliminary                     -- Continuous rule 2
            | otherwise                                          = thirdCase                       -- Continuous rule 3

-- | True if there is any overlap betwet the two intervals
overlaps :: (Double, Double) -> (Double, Double) -> Bool
overlaps leftChild rightChild =
    (rightSmallest < leftLargest) && (rightLargest > leftSmallest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild 


-- | True if one of the intervals falls entirely between the other
-- Assumes there is an overlap
subsetted :: (Double, Double) -> (Double, Double) -> Bool
subsetted leftChild rightChild
    | rightSmallest <= leftSmallest  && rightLargest >= leftLargest  = True
    | leftSmallest  <= rightSmallest && leftLargest  >= rightLargest = True
    | otherwise                                                      = False
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild 


-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible.
-- Does not assume there's an overlap.
--
-- There are six cases:
-- 1: non-intersection with the left < right
-- 2: non-intersection with the left > right
-- 3: intersection but no subsetting, left < right
-- 4: intersection but no subsetting, left > right
-- 5: subsetted, right inside left
-- 6: subsetted, left inside right
intersect :: (Double, Double) -> (Double, Double) -> (Double, Double)
intersect leftChild rightChild
    | not $ leftChild `overlaps` rightChild =
        if leftLargest < rightLargest
            then (leftLargest, rightSmallest)
            else (rightLargest, leftSmallest)
    | subsetted leftChild rightChild =
        if leftLargest > rightLargest
            then (rightSmallest, rightLargest)
            else (leftSmallest, leftLargest)
    | otherwise =
        if leftLargest < rightLargest
            then (rightSmallest, leftLargest)
            else (leftLargest, rightSmallest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild 


-- |
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest value to the largest
-- in both intervals.
--
-- Works for overlapped or subsetted intervals, as well as non-overlapping intervals
union :: (Double, Double) -> (Double, Double)-> (Double, Double)
union leftChild rightChild = (smallestMin, largestMax)
    where
        smallestMin =
            if rightSmallest < leftSmallest
                then rightSmallest
                else leftSmallest
        largestMax =
            if rightLargest < leftLargest
                then leftLargest
                else rightLargest

        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild 


-- TODO: check all inequalities for inclusion of equality
