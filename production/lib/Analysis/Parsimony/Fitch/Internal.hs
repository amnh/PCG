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
-- Fitch (non-additive) character analysis (cost and medians)
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
    { cost         :: Word32                                                     -- cost of the subtree
    , median       :: (DiscreteCharacterDecoration )                             -- for a character state, its min from the left child and its min from the right
    , childMedians :: (DiscreteCharacterDecoration, DiscreteCharacterDecoration) -- so that we can do post order pass with all of Fitch's rules
    }

-- | Used on the post-order (i.e. first) traversal.
fitchPostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
fitchPostOrder charDecoration []               = initialize charDecoration    -- a leaf
fitchPostOrder charDecoration childDecorations = updateNodeDec charDecoration childDecorations

-- | Used on the pre-order (i.e. second) traversal. Either calls 'initializeDirVector' on root or
-- Needs to determine which child it's updating, then sends the appropriate minlist
fitchPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (FitchCharacterDecoration c -> [(Word, d')] -> d')
fitchPreOrder curDecoration (x:y:_)                             = curDecoration                     -- shouldn't be possible, but here for completion
fitchPreOrder curDecoration []                                  = initializeDirVector curDecoration -- is a root
fitchPreOrder curDecoration ((whichChild, parentDecoration):[]) = returnChar
    where
        returnChar
            | whichChild == 0 = updateDirVector parentDecoration (fst (parentDecoration ^. directionalMinVector)) curDecoration -- left child
            | otherwise       = updateDirVector parentDecoration (snd (parentDecoration ^. directionalMinVector)) curDecoration -- right child


updateNodeDec :: DiscreteCharacterDecoration d c => c -> FitchCharacterDecoration c
updateNodeDec curDecoration childDecorations = returnChar
    where
        -- assuming metricity and 0 diagonal
        costList = foldMapWithKey f $ inputDecoration ^. characterAlphabet
            where
                f key alphState
                    | key `testBit` inputChar = [minBound :: Word32]
                    | otherwise               = [maxBound :: Word32] -- Change this if it's actually Doubles.
                    where inputChar = (inputDecoration ^. discreteCharacter)
        returnChar = FitchCharacterDecoration costList [] (minBound :: Word32)

-- |
-- Given current node and its children, does actual calculation of new node value
-- for each character state, for each character on current node. Assumes binary tree.
updateCostVector :: EncodableStaticCharacter c => c -> [FitchCharacterDecoration c] -> FitchCharacterDecoration c
updateCostVector curNodeDecoration []                       = curNodeDecoration    -- Leaf node, so shouldn't get here.
updateCostVector curNodeDecoration (x:[])                   = curNodeDecoration    -- Shouldn't be possible, but here for completion.
updateCostVector curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration -- _Should_ be able to amend this to use non-binary children.
    where
        (charCost, costVector) =
            foldlWithKey' (\(charMin, (leftMin, rightMin)) charState _ -> (charMin, (leftMin : leftChildMin, rightMin : rightChildMin)
                       where
                           charMin = if stateMin < charMin
                               then charMin
                               else stateMin
                           stateMin                      = leftChildMin + rightChildMin
                           (leftChildMin, rightChildMin) =
                               calcCostPerState charState (leftChild ^. discreteCharacter) (rightChild ^. discreteCharacter)
                   ) (maxBound :: Word32, ([],[])) (curNodeDecoration ^. characterAlphabet)
        returnChar = FitchCharacterDecoration costVector [] charCost

initializeDirVector :: FitchCharacterDecoration c -> FitchCharacterDecoration c
initializeDirVector curDecoration = returnChar
    where
        median = foldlWithKey' buildMedian startMedian $ curDecoration ^. minCostVector
        buildMedian acc key charMin
            | charMin == curDecoration ^. minCost = acc `setBit` key
            | otherwise                           = acc
        startMedian = (curDecoration ^. discreteCharacter) `xor` (curDecoration ^. discreteCharacter)
        returnChar = FitchCharacterDecoration (curDecoration ^. minCostVector) median (curDecoration ^. minCost) -- TODO: this is not where median goes. Fix it.

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


-- | Take in a single character state as an Int, which represents an possible unambiguous character state on the parent,
-- and two decorations: the decorations of the two child states.
-- Return the minimum costs of transitioning from each of those two child decorations to the given character.
-- These mins will be saved for use at the next post-order call, to the current parent node's parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we're building medians: the possible character is looped over
-- all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState :: Int -> DiscreteCharacterDecoration c -> DiscreteCharacterDecoration -> (Int, Int)
calcCostPerState inputCharState leftChildDec rightChildDec =
    -- Using keys, fold over alphabet states as Ints. The zipped lists will give minimum accumulated costs for each character state in each child.
    foldlWithKey' (\(leftMin, rightMin) childCharState (accumulatedLeftCharCost, accumulatedRightCharCost) ->
                     leftMin  = if curLeftMin < leftMin
                                then curLeftMin
                                else leftMin
                     rightMin = if curRightMin < rightMin
                                then curRightMin
                                else rightMin
                     where
                         curLeftMin          = leftTransitionCost  + accumulatedLeftCharCost
                         curRightMin         = rightTransitionCost + accumulatedRightCharCost
                         leftTransitionCost  = (leftChildDec  ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState
                         rightTransitionCost = (rightChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState
                 ) (maxBound :: Word32, maxBound :: Word32) zip (leftChildDec ^. minCostVector) (rightChildDec ^. minCostVector)

calcCostAndMedian :: DiscreteCharacterDecoration c -> DiscreteCharacterDecoration c -> (DiscreteCharacterDecoration c, Int)
calcCostAndMedian leftChildDec rightChildDec = returnVal
    where
        returnVal = foldlWithKey' f initializedAcc $ leftChildDec ^. characterAlphabet
        initializedAcc = ((leftChildDec ^. discreteCharacter) `xor` (leftChildDec ^. discreteCharacter), True)
        f (outputChar, isCost) key
            | (leftChildDec ^. discreteCharacter) `testBit` key & (rightChildDec ^. discreteCharacter) `testBit` key =
                (outputChar `setBit` key, False)

