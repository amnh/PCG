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

import Control.Lens
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable


data AdditiveCharacterDecoration c = AdditiveCharacterDecoration
    { minCost           :: Word32                             -- cost of the subtree
    , preliminaryMedian :: c                                  -- held here until final state is determined and we can assign that into discreteCharacter
    , interval          :: c
    , childPrelims      :: (c, c)
    }

-- | Used on the post-order (i.e. first) traversal.
additivePostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
additivePostOrder charDecoration []               = initializeLeaf charDecoration                         -- a leaf
additivePostOrder charDecoration childDecorations = updatePostOrder charDecoration childDecorations

-- | Used on the pre-order (i.e. second) traversal.
additivePreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (AdditiveCharacterDecoration c -> [(Word, d')] -> d')
additivePreOrder curDecoration (x:y:_)                    = curDecoration   -- two parents; shouldn't be possible, but here for completion
additivePreOrder curDecoration []                         = curDecoration   -- is a root TODO: need to change preliminary to final
additivePreOrder curDecoration ((_, parentDecoration):[]) =
    if curDecoration ^. isLeaf    -- TODO: this call probably isn't right
        then curDecoration & discreteCharacter %~ curDecoration ^. preliminaryMedian
        else determineFinalState curDecoration parentDecoration


updatePostOrder :: EncodableStaticCharacter c => c -> [AdditiveCharacterDecoration c] -> AdditiveCharacterDecoration c
updatePostOrder curNodeDecoration []                       = curNodeDecoration    -- Leaf. Here for completion because levaes are filtered out before this call.
updatePostOrder curNodeDecoration (x:[])                   = curNodeDecoration    -- Shouldn't be possible, but here for completion.
updatePostOrder curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration
    where
        (preliminary, newCost)
            | leftChild `overlaps` rightChild = (leftChild `intersect` rightChild, 0)
            | otherwise                       = ((newMin, newMax), totalCost)
        (newMin, newMax)     = getNonOverlapInterval leftChild rightChild
        median               = (newMin, newMax)
        totalCost            = thisNodeCost + (leftChild ^. minCost) + (rightChild ^. minCost)
        thisNodeCost         = newMax - newMin
        returnNodeDecoration = AdditiveCharacterDecoration newCost median (leftChild ^. preliminaryMedian, rightChild ^. preliminaryMedian)

initializeLeaf :: AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c
initializeLeaf curDecoration =
    AdditiveCharacterDecoration 0 label (emptyChar, emptyChar)
    where
        label     = curDecoration ^. discreteCharacter
        emptyChar = label `xor` label        -- TODO: bits 0? Can I optimize this out with the compiler by expanding scope?

determineFinalState :: DiscreteCharacterDecoration c => AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c
determineFinalState curDecoration parentDecoration = finalDecoration
    where
        finalDecoration = curDecoration & interval %. (min, max)
        preliminary     = (curDecoration    ^. interval)
        ancestor        = (parentDecoration ^. interval)
        (left, right)   = curDecoration     ^. childMedians

        (min, max)
            | (ancestor `intersect` preliminary) == ancestor = ancestor                                         -- Additive rule 1
            | curIsUnion    = ancestor `union` preliminary                                                      -- Additive rule 2
            | otherwise     = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor) -- Additive rule 3

union :: EncodableStaticCharacter -> EncodableStaticCharacter -> EncodableStaticCharacter
union l r = l .|. r

overlaps :: AdditiveCharacterDecoration -> AdditiveCharacterDecoration -> Bool
overlaps leftChild rightChild =
    (rightChild ^. smallestChar < leftChild ^. largestChar) && (rightChild ^. largestChar > leftChild ^. smallestChar)

getNonOverlapInterval :: AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c ->
getNonOverlapInterval leftChild rightChild =
    if leftChild ^. largestChar < rightChild ^. smallestChar
        then (leftChild  ^. largestChar, rightChild ^. smallestChar)
        else (rightChild ^. largestChar, leftChild  ^. smallestChar)

-- TODO: assumes there is an intersection. Fix that. Also, one could be proper subset of other.
intersect :: AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c -> AdditiveCharacterDecoration c
intersect leftChild rightChild =
    if leftChild ^. largestChar < rightChild ^. smallestChar
        then (leftChild  ^. largestChar, rightChild ^. smallestChar)
        else (rightChild ^. largestChar, leftChild  ^. smallestChar)

-- TODO: check all inequalities for inclusion of equality; use countLeadingZeroes and countTrailingZeroes.