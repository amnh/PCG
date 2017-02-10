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
additivePreOrder childDecoration (_:_:_)                 = childDecoration   -- multiple parents; shouldn't be possible,
                                                                                --    but here for completion
additivePreOrder childDecoration []                      = childDecoration   -- is a root TODO: need to change preliminary
                                                                             --    to final
additivePreOrder childDecoration [(_, parentDecoration)] =
    if childDecoration ^. isLeaf
        then childDecoration
        else determineFinalState childDecoration parentDecoration


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
                                        then leftInterval `intersect`      rightInterval
                                        else leftInterval `smallestClosed` rightInterval
        (leftInterval, rightInterval) = (leftChild ^. preliminaryInterval, rightChild ^. preliminaryInterval)
        newInterval                   = (newMin, newMax)
        totalCost                     = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
        thisNodeCost                  = if isOverlapping
                                        then 0
                                        else newMax - newMin
        isOverlapping                 = leftInterval `overlaps` rightInterval
        returnNodeDecoration          =
            extendDiscreteToAdditive leftChild totalCost newInterval (leftInterval, rightInterval) False


-- | Initializes a leaf node by copying its current value into its preliminary state. Gives it a minimum cost of 0.
--
-- Used on the postorder pass.
initializeLeaf :: (DiscreteCharacterDecoration d c)
               => d
               -> AdditiveOptimizationDecoration c
initializeLeaf curDecoration =
    extendDiscreteToAdditive curDecoration zero (lower, higher) ((zero,zero),(zero,zero)) True
    where
        label    = curDecoration ^. discreteCharacter
        lower    = (fromIntegral leading :: Word)
        higher   = (fromIntegral (alphLen - 1 - trailing) :: Word)
        alphLen  = symbolCount $ curDecoration ^. discreteCharacter
        trailing = countTrailingZeros label
        leading  = countLeadingZeros label
        zero     = fromIntegral (0 :: Int) :: Word


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
                    -> (AdditiveOptimizationDecoration c)
determineFinalState childDecoration parentDecoration = finalDecoration
    where
        preliminary     = childDecoration  ^. preliminaryInterval
        ancestor        = parentDecoration ^. preliminaryInterval
        (left, right)   = childDecoration  ^. childPrelimIntervals
        thirdCase       = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor)
        (myMin, myMax)
            | (ancestor `intersect` preliminary) == ancestor     = ancestor                        -- Additive rule 1
            | (ancestor `union`     preliminary) == preliminary  = preliminary                     -- Additive rule 2
            | otherwise                                          = thirdCase                       -- Additive rule 3
        interCharacter  = emptyChar      `setBit` (fromIntegral (toInteger myMin :: Integer) :: Int)
        finalCharacter  = interCharacter `setBit` (fromIntegral (toInteger myMax :: Integer) :: Int)
        emptyChar       = emptyStatic $ parentDecoration ^. discreteCharacter
        finalDecoration = childDecoration  &  preliminaryInterval .~ (myMin, myMax) & discreteCharacter .~ finalCharacter


-- | True if there is any overlap between the two intervals
overlaps :: (Word, Word) -> (Word, Word) -> Bool
overlaps leftChild rightChild =
    (rightSmallest <= leftLargest) && (rightLargest >= leftSmallest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild



-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible. Does
-- not assume there's an overlap.
intersect :: (Word, Word) -> (Word, Word) -> (Word, Word)
intersect leftChild rightChild = (max leftSmallest rightSmallest, min leftLargest rightLargest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild



smallestClosed :: (Word, Word) -> (Word, Word) -> (Word, Word)
smallestClosed leftChild rightChild = (min leftLargest rightLargest, max leftSmallest rightSmallest)
{-}    | leftLargest < rightSmallest = (leftLargest, rightSmallest)
    | otherwise                   = (rightLargest, leftSmallest)
 -}  where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild


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


