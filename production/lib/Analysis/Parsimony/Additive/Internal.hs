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

module Analysis.Parsimony.Additive.Internal where

import Bio.Character.Decoration.Additive
--import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
import Control.Lens
-- import Control.Monad  (join)
-- import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty( (:|) ))
-- import Data.Word
-- import Debug.Trace


-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: (DiscreteCharacterDecoration d c)
                  => d
                  -> [AdditiveOptimizationDecoration c]
                  -> AdditiveOptimizationDecoration c
additivePostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration         -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)


-- | Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: EncodableStaticCharacter c
                  => AdditiveOptimizationDecoration c
                  -> [(Word, AdditiveOptimizationDecoration c)]
                  -> AdditiveOptimizationDecoration c
additivePreOrder childDecoration (_:_:_)                 = childDecoration                      -- multiple parents; shouldn't be possible,
                                                                                                --    but here for completion
additivePreOrder childDecoration []                      = finalDecoration                      -- root
    where
        interDecoration = computeFinalDiscrete (childDecoration ^. preliminaryInterval) childDecoration
        finalDecoration = interDecoration & finalInterval .~ childDecoration ^. preliminaryInterval

additivePreOrder childDecoration [(_, parentDecoration)] =
        if   childDecoration ^. isLeaf
        then childDecoration & finalInterval .~ childDecoration ^. preliminaryInterval          -- leaf
        else determineFinalState childDecoration parentDecoration                               -- internal node


-- |
-- Updates the character on the parent of two child nodes to become an 'AdditiveOptimizationDecoration'.
-- Determines the cost by adding the cost of the intersection of the two child nodes, then summing that value
-- with the costs of the two children. The preliminary value of the character is the intersection of the two child intervals.
--
-- Used on the postorder pass.
updatePostOrder :: DiscreteCharacterDecoration d c
                => d
                -> NonEmpty (AdditiveOptimizationDecoration c)
                -> AdditiveOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                     = x                     -- Shouldn't be possible,
                                                                                      --    but here for completion.
updatePostOrder _parentDecoration (leftChild:|(rightChild:_)) = {- trace (show newMin ++ " " ++ show newMax ++ " " ++ show totalCost) $ -}
    returnNodeDecoration  -- Not a leaf.
    where
        (newMin, newMax)              = if isOverlapping
                                        then leftInterval `intersection`   rightInterval
                                        else leftInterval `smallestClosed` rightInterval
        (leftInterval, rightInterval) = (leftChild ^. preliminaryInterval, rightChild ^. preliminaryInterval)
        newInterval                   = (newMin, newMax)
        totalCost                     = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
        thisNodeCost                  = if isOverlapping
                                        then 0
                                        else newMax - newMin
        isOverlapping                 = leftInterval `intersects` rightInterval
        zero                          = fromIntegral (0 :: Int) :: Word
        returnNodeDecoration          =
            extendDiscreteToAdditive leftChild totalCost newInterval (zero, zero) (leftInterval, rightInterval) False


-- | Initializes a leaf node by copying its current value into its preliminary state. Gives it a minimum cost of 0.
--
-- Used on the postorder pass.
initializeLeaf :: (DiscreteCharacterDecoration d c)
               => d
               -> AdditiveOptimizationDecoration c
initializeLeaf curDecoration =
    extendDiscreteToAdditive curDecoration 0 (lower, higher) (0,0) ((0,0),(0,0)) True
    where
        label    = curDecoration ^. discreteCharacter
        alphLen  = symbolCount label
        trailing = countTrailingZeros label
        leading  = countLeadingZeros  label
        lower    = toEnum leading
        higher   = toEnum (alphLen - 1 - trailing)


-- | Uses the preliminary intervals of a node, its parents, and its children. Follows the three rules of Fitch,
-- modified for additive characters: 1) If the intersection of the current node's character with its parent == the
-- parent interval, use the parent interval; 2) If the union of those two characters == the child, then use the
-- child; 3) Otherwise, find the intersections of the parent and each of the children, union them, then union that
-- with the parent.
--
-- Used on the preorder pass.
determineFinalState :: EncodableStaticCharacter c
                    => AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
determineFinalState childDecoration parentDecoration = finalDecoration
    where
        curIsSuperset   = (ancestor `intersection` preliminary) == ancestor
        finalDecoration = computeFinalDiscrete (myMin, myMax) childDecoration
        preliminary     = childDecoration  ^. preliminaryInterval
        ancestor        = parentDecoration ^. finalInterval
        (left, right)   = childDecoration  ^. childPrelimIntervals
        chi             = (leftUnionright `union` preliminary) `intersection` ancestor
        leftUnionright  = left `union` right
        prelimClosestA  = closestState preliminary ancestor
        childsCloseestA = closestState leftUnionright ancestor
        (myMin, myMax)
            | curIsSuperset = ancestor                                                              -- Additive rule 1
            | leftUnionright `intersects` ancestor  =                                               -- Additive rule 2
                if   chi `intersects` preliminary
                then chi
                else largestClosed (closestState preliminary chi) chi
            | otherwise = (min prelimClosestA childsCloseestA, max prelimClosestA childsCloseestA)  -- Additive rule 3


computeFinalDiscrete :: EncodableStaticCharacter c
                    => (Word,Word)
                    -> AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
computeFinalDiscrete (myMin, myMax) childDecoration = finalDecoration
    where
        interCharacter  = emptyChar      `setBit` (fromIntegral (toInteger myMin :: Integer) :: Int)
        finalCharacter  = interCharacter `setBit` (fromIntegral (toInteger myMax :: Integer) :: Int)
        emptyChar       = emptyStatic $ childDecoration ^. discreteCharacter
        finalDecoration = childDecoration & finalInterval .~ (myMin, myMax) & discreteCharacter .~ finalCharacter

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


