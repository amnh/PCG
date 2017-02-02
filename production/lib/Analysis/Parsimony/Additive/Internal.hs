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
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty( (:|) ))
--import Data.Word
import Debug.Trace


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
        (newMin, newMax)              = leftInterval `intersect` rightInterval
        (leftInterval, rightInterval) = join bimap (^. preliminaryInterval) (leftChild, rightChild)
        newInterval                   = (newMin, newMax)
        totalCost                     = thisNodeCost + (leftChild ^. characterCost) + (rightChild ^. characterCost)
        thisNodeCost                  = newMax - newMin
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
        label   = curDecoration ^. discreteCharacter
        lower   = fromIntegral (countTrailingZeros label) :: Word
        higher  = fromIntegral (alphLen - 1 - countLeadingZeros label) :: Word
        alphLen = symbolCount $ curDecoration ^. discreteCharacter
        zero    = fromIntegral (0 :: Int) :: Word


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


-- | True if one of the intervals falls entirely within the other
--
-- Assumes there is an overlap.
subsetted :: (Word, Word) -> (Word, Word) -> Bool
subsetted leftChild rightChild
    | rightSmallest <= leftSmallest  && rightLargest >= leftLargest  = True
    | leftSmallest  <= rightSmallest && leftLargest  >= rightLargest = True
    | otherwise                                                      = False
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild


-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible. Does
-- not assume there's an overlap.
--
-- There are seven cases:
-- 1: non-intersection with the left < right
-- 2: non-intersection with the left > right
-- 3: intersection but no subsetting, left < right
-- 4: intersection but no subsetting, left > right
-- 5: subsetted, one of two is unambiguous
-- 6: subsetted, right inside left
-- 7: subsetted, left inside right
intersect :: (Word, Word) -> (Word, Word) -> (Word, Word)
intersect leftChild rightChild
    | not $ leftChild `overlaps` rightChild =
        -- trace ("no overlap " ++ debugString) $
        if leftLargest < rightSmallest
            then (leftLargest, rightSmallest)
            else (rightLargest, leftSmallest)
    | subsetted leftChild rightChild =
        -- trace ("subsetted   " ++ debugString) $
        subsetCases
    | otherwise =
        -- trace ("intersecion " ++ debugString) $
        if leftLargest < rightLargest
            then (rightSmallest, leftLargest)
            else (leftSmallest, rightLargest)
    where
        (rightSmallest, rightLargest) = rightChild
        ( leftSmallest,  leftLargest) = leftChild
        debugString = (show . unlines $ fmap show [leftSmallest, leftLargest, rightSmallest, rightLargest])
        subsetCases
            | leftLargest  == leftSmallest  = (leftSmallest,  leftLargest)  -- smallest closed interval is 0
            | rightLargest == rightSmallest = (rightSmallest, rightLargest) -- smallest closed interval is 0
            | leftLargest  >= rightLargest  = (rightSmallest, rightLargest) -- smallest closed interval is smallest of two
            | otherwise                     = (leftSmallest, leftLargest)   -- smallest closed interval is smallest of two



-- |
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest possible
-- value to the largest possible, considering the values in both intervals.
--
-- Works for overlapped or subsetted intervals, as well as non-overlapping intervals
union :: (Word, Word) -> (Word, Word)-> (Word, Word)
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
