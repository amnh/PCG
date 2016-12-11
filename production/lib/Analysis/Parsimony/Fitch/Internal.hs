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
    { subtreeCost       :: Word32                                                     -- cost of the subtree
    , preliminaryMedian :: (DiscreteCharacterDecoration)                              -- held here until final state is determined and we can assign that into discreteCharacter
    , childMedians      :: (DiscreteCharacterDecoration, DiscreteCharacterDecoration) -- (left, right) so that we can do post order pass with all of Fitch's rules
    }

-- | Used on the post-order (i.e. first) traversal.
fitchPostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
fitchPostOrder charDecoration []               = initializeLeaf charDecoration    -- a leaf
fitchPostOrder charDecoration childDecorations = updateNodeDecoration charDecoration childDecorations

-- | Used on the pre-order (i.e. second) traversal. Either calls 'initializeDirVector' on root or
-- Needs to determine which child it's updating, then sends the appropriate minlist
fitchPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (FitchCharacterDecoration c -> [(Word, d')] -> d')
fitchPreOrder curDecoration (x:y:_)                             = curDecoration                     -- shouldn't be possible, but here for completion
fitchPreOrder curDecoration []                                  =
    initializeDirVector curDecoration -- is a root
fitchPreOrder curDecoration ((whichChild, parentDecoration):[]) = returnChar
    where
        returnChar
            | whichChild == 0 = updateDirVector parentDecoration (fst (parentDecoration ^. directionalMinVector)) curDecoration -- left child
            | otherwise       = updateDirVector parentDecoration (snd (parentDecoration ^. directionalMinVector)) curDecoration -- right child


calcCostAndMedian :: EncodableStaticCharacter c => c -> [FitchCharacterDecoration c] -> FitchCharacterDecoration c
calcCostAndMedian curNodeDecoration []                       = curNodeDecoration    -- Leaf. Here for completion because levaes are filtered out before this call.
calcCostAndMedian curNodeDecoration (x:[])                   = curNodeDecoration    -- Shouldn't be possible, but here for completion.
calcCostAndMedian curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration
    where
        returnNodeDecoration   = FitchCharacterDecoration cost median (leftChild ^. preliminaryMedian, rightChild ^. preliminaryMedian)
        (cost, median) = calcCostAndMedian leftChild rightChild
        returnVal            = foldlWithKey' f initializedAcc $ leftChildDec ^. characterAlphabet

        initializedAcc       = (emptyChar, True)
        emptyChar            = (leftChildDec ^. discreteCharacter) `xor`     (leftChildDec ^. discreteCharacter)
        isSet decoration key = (decoration   ^. discreteCharacter) `testBit` key
        indel l r k          = (isSet l k) `xor` (isSet r k)
        noSub l r k          = (isSet l k) &&    (isSet r k)
        f (inChar, isCost) key
            | noSub =
                if isCost
                    then (emptyChar `setBit` key, False)
                    else (inChar    `setBit` key, isCost)
            | indel leftChildDec rightChildDec key =
                if isCost
                    then (inChar `setBit` key, isCost)
                    else (inChar,              isCost)
            | otherwise  = (inChar, isCost)


initializeLeaf :: FitchCharacterDecoration c -> FitchCharacterDecoration c
initializeLeaf curDecoration =
    FitchCharacterDecoration 0 label (emptyChar, emptyChar)
        where
            label     = curDecoration ^. discreteCharacter
            emptyChar = label `xor` label

-- |
-- Takes two decorations in, a child and a parent, and calculates the median character value of the child. For each possible character state,
-- this value is based on whether that character state in the child is on one of the min-cost paths from the root to the leaves.
-- It relies on dynamic programming to do so, using the minimum tuple in the parent to determine whether that character state can participate
-- in the final median. Using the left child as a template, the character state is part of the median if, for some state in the parent,
-- parCharState_minCost_left == childCharState_minCost + TCM(childCharState, parCharState).
updateDirVector :: DiscreteCharacterDecoration c => FitchCharacterDecoration c -> FitchCharacterDecoration c -> FitchCharacterDecoration c
updateDirVector parentDecoration parentMins childDecoration = returnChar
    where
        median = foldlWithKey' (\acc parentCharState parentCharMin ->
                                    if parentCharMin == parentDecoration ^. minCost
                                    then foldlWithKey' buildMedian acc parentCharMin $ curDecoration ^. minCostVector
                                    else acc
                               ) startMedian parentMins

        buildMedian acc childCharState parentCharState charMin
            | charMin == curDecoration ^. minCost + (curDecoration ^. characterSymbolTransitionCostMatrixGenerator) childCharState parentCharState =
                acc `setBit` childCharState
            | otherwise                           = acc
        startMedian = (curDecoration ^. discreteCharacter) `xor` (curDecoration ^. discreteCharacter)
        parentMin   = if whichChild == 0
                        then parentDecoration ^. leftChildMin
                        else parentDecoration ^. rightChildMin
        returnChar  = curDecoration ^. discreteCharacter = median