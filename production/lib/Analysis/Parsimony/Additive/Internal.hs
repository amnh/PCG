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
-----------------------------------------------------------------------------

module Analysis.Parsimony.Additive.Internal where

import Bio.Character.Decoration.Additive
--import Bio.Character.Decoration.Discrete
--import Bio.Character.Encodable
import Control.Lens
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty( (:|) ))
--import Data.Word


-- data AdditiveOptimizationDecoration c = AdditiveOptimizationDecoration
--     { minCost           :: Word      -- cost of the subtree
--     , preliminaryInterval :: c           -- held here until final state is determined and we can assign that into discreteCharacter
--     , isLeaf            :: Bool
--     , interval          :: c
--     , childPrelims      :: (c, c)
--     }

-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: (DiscreteCharacterDecoration d c, FiniteBits c)
                  => d
                  -> [AdditiveOptimizationDecoration c]
                  -> AdditiveOptimizationDecoration c
additivePostOrder parentDecoration xs =
    case xs of
        []   -> initializeLeaf  parentDecoration         -- a leaf
        y:ys -> updatePostOrder parentDecoration (y:|ys)

-- | Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: AdditiveOptimizationDecoration c
                  -> [(Word, AdditiveOptimizationDecoration c)]
                  -> AdditiveOptimizationDecoration c
additivePreOrder childDecoration (_x:_y:_)                  = childDecoration   -- multiple parents; shouldn't be possible,
                                                                                --    but here for completion
additivePreOrder childDecoration []                         = childDecoration   -- is a root TODO: need to change preliminary
                                                                                --    to final
additivePreOrder childDecoration ((_, parentDecoration):[]) =
    if childDecoration ^. isLeaf
        then childDecoration
        else determineFinalState childDecoration parentDecoration


updatePostOrder :: DiscreteCharacterDecoration d c
                => d
                -> NonEmpty (AdditiveOptimizationDecoration c)
                -> AdditiveOptimizationDecoration c
updatePostOrder _parentDecoration (x:|[])                    = x                     -- Shouldn't be possible,
                                                                                     --    but here for completion.
updatePostOrder  parentDecoration (leftChild:|(rightChild:_)) = returnNodeDecoration  -- Not a leaf.
    where
        (newMin, newMax)              = leftInterval `intersect` rightInterval
        (leftInterval, rightInterval) = join bimap (^. preliminaryInterval) (leftChild, rightChild)
        newInterval                   = (newMin, newMax)
        totalCost                     = thisNodeCost + (leftChild ^. minCost) + (rightChild ^. minCost)
        thisNodeCost                  = newMax - newMin
        returnNodeDecoration          =
            extendDiscreteToAdditive parentDecoration totalCost newInterval (leftInterval, rightInterval) False

initializeLeaf :: (DiscreteCharacterDecoration d c, FiniteBits c)
               => d
               -> AdditiveOptimizationDecoration c
initializeLeaf curDecoration =
    extendDiscreteToAdditive curDecoration zero (lower, higher) ((zero,zero),(zero,zero)) True
    where
        label  = curDecoration ^. discreteCharacter
        lower  = fromIntegral (countTrailingZeros label) :: Word
        higher = fromIntegral (countLeadingZeros  label) :: Word
        zero   = fromIntegral (0 :: Int) :: Word

determineFinalState :: AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
                    -> AdditiveOptimizationDecoration c
determineFinalState childDecoration parentDecoration = finalDecoration
    where
        finalDecoration = childDecoration  &  preliminaryInterval .~ (thisMin, thisMax)
        preliminary     = childDecoration  ^. preliminaryInterval
        ancestor        = parentDecoration ^. preliminaryInterval
        (left, right)   = childDecoration  ^. childPrelimIntervals
        thirdCase       = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor)
        (thisMin, thisMax)
            | (ancestor `intersect` preliminary) == ancestor     = ancestor                        -- Additive rule 1
            | (ancestor `union`     preliminary) == preliminary  = preliminary                     -- Additive rule 2
            | otherwise                                          = thirdCase                       -- Additive rule 3

-- | True if there is any overlap betwet the two intervals
overlaps :: (Word, Word) -> (Word, Word) -> Bool
overlaps leftChild rightChild =
    (rightSmallest < leftLargest) && (rightLargest > leftSmallest)
    where
        rightSmallest = fst (rightChild)
        rightLargest  = snd (rightChild)
        leftSmallest  = fst (leftChild)
        leftLargest   = snd (leftChild)

-- | True if one of the intervals falls entirely between the other
-- Assumes there is an overlap
subsetted :: (Word, Word) -> (Word, Word) -> Bool
subsetted leftChild rightChild
    | rightSmallest <= leftSmallest  && rightLargest >= leftLargest  = True
    | leftSmallest  <= rightSmallest && leftLargest  >= rightLargest = True
    | otherwise                                                      = False
    where
        rightSmallest = fst (rightChild)
        rightLargest  = snd (rightChild)
        leftSmallest  = fst (leftChild)
        leftLargest   = snd (leftChild)


-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible.
--
-- There are six cases:
-- 1: non-intersection with the left < right
-- 2: non-intersection with the left > right
-- 3: intersection but no subsetting, left < right
-- 4: intersection but no subsetting, left > right
-- 5: subsetted, right inside left
-- 6: subsetted, left inside right
intersect :: (Word, Word) -> (Word, Word) -> (Word, Word)
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
        rightSmallest = fst (rightChild)
        rightLargest  = snd (rightChild)
        leftSmallest  = fst (leftChild)
        leftLargest   = snd (leftChild)

-- |
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest value to the largest
-- in both intervals.
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

        rightSmallest = fst (rightChild)
        rightLargest  = snd (rightChild)
        leftSmallest  = fst (leftChild)
        leftLargest   = snd (leftChild)


-- TODO: check all inequalities for inclusion of equality