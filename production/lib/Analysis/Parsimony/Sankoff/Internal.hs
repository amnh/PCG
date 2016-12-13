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
import Data.List.NonEmpty (NonEmpty( (:|) ))
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
sankoffPostOrder charDecoration xs =
    case xs of
        []   -> initializeCostVector charDecoration              -- is a leaf
        y:ys -> updateCostVector     charDecoration (y:|ys)


-- | Used on the pre-order (i.e. second) traversal. Either calls 'initializeDirVector' on root or
-- Needs to determine which child it's updating, then sends the appropriate minlist
sankoffPreOrder  :: (-- DiscreteCharacterDecoration d c
--                    , HasCharacterTransitionCostMatrix d (c -> c -> (c, Int))
                      EncodableStaticCharacter c
                    ) => SankoffOptimizationDecoration c
                      -> [(Word, SankoffOptimizationDecoration c)]
                      -> SankoffOptimizationDecoration c
sankoffPreOrder childDecoration []                                 = initializeDirVector childDecoration -- is a root
sankoffPreOrder childDecoration ((whichChild, parentDecoration):_) = resultDecoration $
    case whichChild of
        0 -> fst
        _ -> snd
    where
        resultDecoration f = updateDirVector parentDecoration childDecoration . f $ parentDecoration ^. directionalMinVector


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
        returnChar = extendDiscreteToSankoff inputDecoration costList ([],[]) (minBound :: Word)


-- |
-- Given current node and its children, does actual calculation of new node value
-- for each character state.
--
-- Assumes binary tree.
updateCostVector :: ( EncodableStaticCharacter c
                    , DiscreteCharacterDecoration d c
                    ) => d
                      -> NonEmpty (SankoffOptimizationDecoration c)
                      -> SankoffOptimizationDecoration c
updateCostVector parentDecoration (x:|[])                   = x                    -- Shouldn't be possible, but here for completion.
updateCostVector parentDecoration (leftChild:|rightChild:_) = returnNodeDecoration -- _Should_ be able to amend this to use non-binary children.
    where
        (costVector, dirCostVector, charCost) = foldr findMins initialAccumulator [0..length(parentDecoration ^. characterAlphabet)]
        initialAccumulator     = ([], ([],[]), maxBound :: Word)  -- (current minCost, (leftMin, rightMin))
        returnNodeDecoration   = extendDiscreteToSankoff parentDecoration costVector dirCostVector $ fromIntegral charCost

        findMins charState (stateMins, (leftMin, rightMin), curMin) = returnVal
             where
                 charMin = if stateMin < curMin
                           then curMin
                           else stateMin
                 stateMin                      = leftChildMin + rightChildMin
                 (leftChildMin, rightChildMin) = calcCostPerState charState leftChild rightChild
                 returnVal                     = (stateMin : stateMins, (leftChildMin : leftMin, rightChildMin : rightMin), charMin)


initializeDirVector :: SankoffDecoration s c => s -> s
initializeDirVector curDecoration = curDecoration
    -- where
    --     median = foldlWithKey' buildMedian startMedian $ curDecoration ^. minCostVector
    --     buildMedian acc key charMin
    --         | charMin == curDecoration ^. minCost = acc `setBit` key
    --         | otherwise                           = acc
    --     startMedian = (curDecoration ^. discreteCharacter) `xor` (curDecoration ^. discreteCharacter)
    --     returnChar  = curDecoration & directionalMinVector .~ median

--          SankoffOptimizationDecoration (curDecoration ^. minCostVector) median (curDecoration ^. minCost) -- TODO: this is not where median goes. Fix it.


-- |
-- Takes two decorations in, a child and a parent, and calculates the median character value of the child.
-- For each possible character state this value is based on whether that character state in the child is on
-- one of the min-cost paths from the root to the leaves. It relies on dynamic programming to do so,
-- using the minimum tuple in the parent to determine whether that character state can participate
-- in the final median. Using the left child as a template, the character state is part of the median if,
-- for some state in the parent,
-- parCharState_minCost_left == childCharState_minCost + TCM(childCharState, parCharState).
--
-- Used on second, pre-order, pass.
updateDirVector :: Bits c
                => SankoffOptimizationDecoration c
                -> SankoffOptimizationDecoration c
                -> [Word]
                -> SankoffOptimizationDecoration c
updateDirVector parentDecoration childDecoration parentMins  = returnChar
    where
        median = foldlWithKey' (\acc parentCharState parentCharMin ->
                                    if parentCharMin == parentDecoration ^. minCost
                                    then foldlWithKey' (buildMedian parentCharState) acc $ parentDecoration ^. minCostVector
                                    else acc
                               ) startMedian parentMins

        buildMedian parentCharState acc childCharState charMin
            | charMin == totalCost childCharState parentCharState = acc `setBit` childCharState
            | otherwise                                           = acc
        tcmCostAsWord childState parState = fromIntegral $ (parentDecoration ^. characterSymbolTransitionCostMatrixGenerator) childState parState
        totalCost childState parState     = parentDecoration ^. minCost + fromIntegral (tcmCostAsWord childState parState)
        startMedian                       = (parentDecoration ^. discreteCharacter) `xor` (parentDecoration ^. discreteCharacter)
        returnChar                        = childDecoration & discreteCharacter .~ median


-- | Take in a single character state as an Int—which represents an unambiguous character state on the parent—
-- and two decorations: the decorations of the two child states.
-- Return the minimum costs of transitioning from each of those two child decorations to the given character.
-- These mins will be saved for use at the next post-order call, to the current parent node's parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we're building medians: the possible character is looped over
-- all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState :: Int -> SankoffOptimizationDecoration c -> SankoffOptimizationDecoration c -> (Word, Word)
calcCostPerState inputCharState leftChildDec rightChildDec = retVal
    where
        -- Using keys, fold over alphabet states as Ints. The zipped lists will give minimum accumulated costs for each character state in each child.
        retVal = foldlWithKey' findMins initialAccumulator zippedCostList
        findMins :: (Word, Word) -> Int -> (Word, Word) -> (Word, Word)
        findMins (initLeftMin, initRightMin) childCharState (accumulatedLeftCharCost, accumulatedRightCharCost) = (leftMin, rightMin)
            where
                leftMin             = if curLeftMin < initLeftMin
                                      then curLeftMin
                                      else initLeftMin
                rightMin            = if curRightMin < initRightMin
                                      then curRightMin
                                      else initRightMin
                curLeftMin          = fromIntegral leftTransitionCost  + accumulatedLeftCharCost
                curRightMin         = fromIntegral rightTransitionCost + accumulatedRightCharCost
                leftTransitionCost  = ( leftChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState
                rightTransitionCost = (rightChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState

        initialAccumulator = (maxBound :: Word, maxBound :: Word)
        zippedCostList     = zip (leftChildDec ^. minCostVector) (rightChildDec ^. minCostVector)
