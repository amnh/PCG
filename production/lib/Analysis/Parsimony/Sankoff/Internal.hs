-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Sankoff.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Sankoff character analysis (cost and median)
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Sankoff.Internal where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Metric
import Bio.Character.Encodable
import Control.Lens
import Data.Bits
import Data.Key
import Data.Word
import Prelude hiding (zip)

{-
data SankoffPostOrderResult c = SankoffPostOrderResult
    { minCostVector        :: [Word]             -- overall min for each character state
    , directionalMinVector :: ([Word], [Word]) -- for a character state, its min from the left child and its min from the right
    , minCost              :: Word
    }
-}

-- data SankoffOptimizationDecoration c = SankoffOptimizationDecoration c


-- | Used on the post-order (i.e. first) traversal.
sankoffPostOrder :: ( EncodableStaticCharacter c
                    , DiscreteCharacterDecoration d c
                    ) => d
                      -> [SankoffOptimizationDecoration c]
                      ->  SankoffOptimizationDecoration c
sankoffPostOrder charDecoration []               = initializeCostVector charDecoration              -- is a leaf
sankoffPostOrder charDecoration childDecorations = updateCostVector charDecoration childDecorations


-- | Used on the pre-order (i.e. second) traversal. Either calls 'initializeDirVector' on root or
-- Needs to determine which child it's updating, then sends the appropriate minlist
sankoffPreOrder  :: (-- DiscreteCharacterDecoration d c
--                    , HasCharacterTransitionCostMatrix d (c -> c -> (c, Int))
                      EncodableStaticCharacter c
                    ) => SankoffOptimizationDecoration c
                      -> [(Word, SankoffOptimizationDecoration c)]
                      -> SankoffOptimizationDecoration c
sankoffPreOrder curDecoration []                                 = initializeDirVector curDecoration -- is a root
sankoffPreOrder curDecoration ((whichChild, parentDecoration):_) = returnChar
    where
        returnChar
            | whichChild == 0 = updateDirVector parentDecoration (fst (parentDecoration ^. directionalMinVector)) curDecoration -- left child
            | otherwise       = updateDirVector parentDecoration (snd (parentDecoration ^. directionalMinVector)) curDecoration -- right child


-- | Before post-order can actually occur, must initialize leaf vectors with values as such:
-- Given \(n\) character states, for a given character \(i_c\) on leaf \(i\), there are \(2^n - 1)
-- possible characters, including ambiguous characters. For extant character states \(i_c_x\) on
-- the leaf, and for each possible character state, \(i\)
-- \[ cost(i_c) =
--       \] \(i \elem s_x\), etc...
-- TODO: finish above comment once MathJax is working
initializeCostVector :: ( Bits c,
                          DiscreteCharacterDecoration d c
                        ) => d
                          -> SankoffOptimizationDecoration c
initializeCostVector inputDecoration = returnChar
    where
        -- assuming metricity and 0 diagonal
        costList = foldMapWithKey f $ inputDecoration ^. characterAlphabet
            where
                f key alphState
                    | inputChar `testBit` key = [minBound :: Word]
                    | otherwise               = [maxBound :: Word] -- Change this if it's actually Doubles.
                    where inputChar = inputDecoration ^. discreteCharacter
        returnChar = extendToSankoff inputDecoration costList ([],[]) (minBound :: Word)


-- |
-- Given current node and its children, does actual calculation of new node value
-- for each character state, for each character on current node. Assumes binary tree.
updateCostVector :: ( EncodableStaticCharacter c
                    , DiscreteCharacterDecoration d c
                    ) => d
                      -> [SankoffOptimizationDecoration c]
                      -> SankoffOptimizationDecoration c
updateCostVector curNodeDecoration []                       = curNodeDecoration    -- Leaf node, so shouldn't get here.
updateCostVector curNodeDecoration [x]                      = curNodeDecoration    -- Shouldn't be possible, but here for completion.
updateCostVector curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration -- _Should_ be able to amend this to use non-binary children.
    where
        (charCost, costVector) =
            foldlWithKey' findMins initialAccumulator (curNodeDecoration ^. characterAlphabet)
        initialAccumulator = (maxBound :: Word, ([],[]))
        findMins (charMin, (leftMin, rightMin)) = returnVal
             where
                 charMin = if stateMin < charMin
                           then charMin
                           else stateMin
                 stateMin                      = leftChildMin + rightChildMin
                 (leftChildMin, rightChildMin) = calcCostPerState charState (leftChild ^. discreteCharacter) (rightChild ^. discreteCharacter)
                 returnVal = (charMin, (leftMin : leftChildMin, rightMin : rightChildMin))

                 returnNodeDecoration = extendToSankoff curNodeDecoration costVector ([],[]) charCost


initializeDirVector :: SankoffOptimizationDecoration c -> SankoffOptimizationDecoration c
initializeDirVector curDecoration = returnChar
    where
        median = foldlWithKey' buildMedian startMedian $ curDecoration ^. minCostVector
        buildMedian acc key charMin
            | charMin == curDecoration ^. minCost = acc `setBit` key
            | otherwise                           = acc
        startMedian = (curDecoration ^. discreteCharacter) `xor` (curDecoration ^. discreteCharacter)
        returnChar = curDecoration & directionalMinVector %~ median

--          SankoffOptimizationDecoration (curDecoration ^. minCostVector) median (curDecoration ^. minCost) -- TODO: this is not where median goes. Fix it.


-- |
-- Takes two decorations in, a child and a parent, and calculates the median character value of the child. For each possible character state,
-- this value is based on whether that character state in the child is on one of the min-cost paths from the root to the leaves.
-- It relies on dynamic programming to do so, using the minimum tuple in the parent to determine whether that character state can participate
-- in the final median. Using the left child as a template, the character state is part of the median if, for some state in the parent,
-- parCharState_minCost_left == childCharState_minCost + TCM(childCharState, parCharState).
updateDirVector :: SankoffOptimizationDecoration c
                -> [Word]
                -> SankoffOptimizationDecoration c
                -> SankoffOptimizationDecoration c
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
        returnChar  = curDecoration & discreteCharacter %~ median


-- | Take in a single character state as an Int, which represents an possible unambiguous character state on the parent,
-- and two decorations: the decorations of the two child states.
-- Return the minimum costs of transitioning from each of those two child decorations to the given character.
-- These mins will be saved for use at the next post-order call, to the current parent node's parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we're building medians: the possible character is looped over
-- all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState :: Int -> SankoffOptimizationDecoration c -> SankoffOptimizationDecoration c -> (Int, Int)
calcCostPerState inputCharState leftChildDec rightChildDec = retVal
    where
        -- Using keys, fold over alphabet states as Ints. The zipped lists will give minimum accumulated costs for each character state in each child.
        retVal = foldlWithKey' findMins initialAccumulator zippedCostList

        findMins (leftMin, rightMin) childCharState (accumulatedLeftCharCost, accumulatedRightCharCost) = (leftMin, rightMin)
            where
                leftMin             = if curLeftMin < leftMin
                                      then curLeftMin
                                      else leftMin
                rightMin            = if curRightMin < rightMin
                                      then curRightMin
                                      else rightMin
                curLeftMin          = leftTransitionCost  + accumulatedLeftCharCost
                curRightMin         = rightTransitionCost + accumulatedRightCharCost
                leftTransitionCost  = ( leftChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState
                rightTransitionCost = (rightChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState

        initialAccumulator = (maxBound :: Word, maxBound :: Word)
        zippedCostList = zip (leftChildDec ^. minCostVector) (rightChildDec ^. minCostVector)
