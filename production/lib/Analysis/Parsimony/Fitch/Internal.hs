-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Fitch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard Fitch (non-additive) character analysis (cost and medians).
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

import Control.Lens
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable

data FitchCharacterDecoration c = FitchCharacterDecoration
    { subtreeCost       :: Word32                                               -- cost of the subtree
    , preliminaryMedian :: (EncodableStaticCharacter)                           -- held here until final state is determined and we can assign that into discreteCharacter
    , childMedians      :: (EncodableStaticCharacter, EncodableStaticCharacter) -- (left, right) so that we can do post order pass with all of Fitch's rules
    }

-- | Used on the post-order (i.e. first) traversal.
fitchPostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
fitchPostOrder charDecoration []               = initializeLeaf charDecoration                         -- a leaf
fitchPostOrder charDecoration childDecorations = updatePostOrder charDecoration childDecorations

-- | Used on the pre-order (i.e. second) traversal.
fitchPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (FitchCharacterDecoration c -> [(Word, d')] -> d')
fitchPreOrder curDecoration (x:y:_)                    = curDecoration   -- two parents; shouldn't be possible, but here for completion
fitchPreOrder curDecoration []                         = curDecoration   -- is a root TODO: need to change preliminary to final
fitchPreOrder curDecoration ((_, parentDecoration):[]) =
    if curDecoration ^. isLeaf    -- TODO: this call probably isn't right
        then curDecoration & discreteCharacter %~ curDecoration ^. preliminaryMedian
        else determineFinalState curDecoration parentDecoration


updatePostOrder :: EncodableStaticCharacter c => c -> [FitchCharacterDecoration c] -> FitchCharacterDecoration c
updatePostOrder curNodeDecoration []                       = curNodeDecoration    -- Leaf. Here for completion because levaes are filtered out before this call.
updatePostOrder curNodeDecoration (x:[])                   = curNodeDecoration    -- Shouldn't be possible, but here for completion.
updatePostOrder curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration
    where
        returnNodeDecoration   = FitchCharacterDecoration totalCost median (leftChild ^. preliminaryMedian, rightChild ^. preliminaryMedian)
        (median, thisNodeCost) = foldlWithKey' f initializedAcc leftChildDec rightChild

        initializedAcc         = (emptyChar, 1)
        emptyChar              = (leftChildDec ^. discreteCharacter) `xor`     (leftChildDec ^. discreteCharacter)
        isSet decoration key   = (decoration   ^. discreteCharacter) `testBit` key
        indel l r k            = (isSet l k) `xor` (isSet r k)
        noSub l r k            = (isSet l k) &&    (isSet r k)
        totalCost              = thisNodeCost + (leftChild ^. subtreeCost) + (rightChild ^. subtreeCost)
        f (inChar, cost) key
            | noSub leftChildDec rightChildDec key =
                if cost > 0
                    then (emptyChar `setBit` key, 0)
                    else (inChar    `setBit` key, 0)
            | indel leftChildDec rightChildDec key =
                if cost > 0
                    then (inChar `setBit` key, cost)
                    else (inChar,              cost)
            | otherwise = (inChar, cost)


initializeLeaf :: FitchCharacterDecoration c -> FitchCharacterDecoration c
initializeLeaf curDecoration =
    FitchCharacterDecoration 0 label (emptyChar, emptyChar)
        where
            label     = curDecoration ^. discreteCharacter
            emptyChar = label `xor` label

determineFinalState :: DiscreteCharacterDecoration c => FitchCharacterDecoration c -> FitchCharacterDecoration c -> FitchCharacterDecoration c
determineFinalState curDecoration parentDecoration = finalDecoration
    where
        curIsSuperset = foldlWithKey (\acc k _ -> if (ancestor `testBit` k) && (preliminary `testBit` k)
                                                       then acc && False
                                                       else acc && True
                                                      ) True $ curDecoration ^. characterAlphabet
        curIsUnion = foldlWithKey (\acc k _ -> acc && !(left || right `xor` preliminary) `testBit` k)) -- preliminary is 0 if both are 0, 1 otherwise
                                                                                                       -- TODO: see if this short-circuits; otherwise rewrite
                                                                                                       -- doing testbit three times and then logical operations

                                                      ) True $ curDecoration ^. characterAlphabet
        finalDecoration = curDecoration    &  discreteCharacter %. median
        preliminary     = curDecoration    ^. preliminaryMedian
        ancestor        = parentDecoration ^. discreteCharacter
        (left, right)   = curDecoration    ^. childMedians
        union     l r   = l .|. r
        intersect l r   = l .&. r
        median
            | curIsSuperset = ancestor                                                                          -- Fitch rule 1
            | curIsUnion    = ancestor `union` preliminary                                                      -- Fitch rule 2
            | otherwise     = ancestor `union` (left `intersect` ancestor) `union` (right `intersect` ancestor) -- Fitch rule 3

        isOverlap answer bit =