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
import Data.ExtendedNatural
import Data.Key
import Data.List.NonEmpty (NonEmpty( (:|) ))
import qualified Data.TCM as TCM
import Data.Word
import Prelude hiding (zip)

import Debug.Trace


-- | Used on the post-order (i.e. first) traversal.
sankoffPostOrder :: DiscreteCharacterDecoration d c
                 => d
                 -> [SankoffOptimizationDecoration c]
                 ->  SankoffOptimizationDecoration c
sankoffPostOrder charDecoration xs =
    case xs of
        []   -> initializeCostVector charDecoration              -- is a leaf
        y:ys -> updateCostVector     charDecoration (y:|ys)


-- | Used on the pre-order (i.e. second) traversal. Either calls 'initializeDirVector' on root or updateDirectionalMins.
-- Needs to determine which child it's updating, then sends the appropriate minlist
sankoffPreOrder :: EncodableStaticCharacter c
                => SankoffOptimizationDecoration c
                -> [(Word, SankoffOptimizationDecoration c)]
                -> SankoffOptimizationDecoration c
sankoffPreOrder childDecoration []                                 = childDecoration -- is a root
sankoffPreOrder childDecoration ((whichChild, parentDecoration):_) = resultDecoration $
    case whichChild of
        0 -> fst
        _ -> snd
    where
        resultDecoration f = updateDirectionalMins parentDecoration childDecoration . f $ parentDecoration ^. directionalMinVector


-- | Before post-order can actually occur, must initialize leaf vectors with values as such:
-- Given \(n\) character states, for a given character \(i_c\) on leaf \(i\), there are \(2^n - 1)
-- possible characters, including ambiguous characters. For extant character states \(i_c_x\) on
-- the leaf, and for each possible character state, if that character state is extant on the leaf, give
-- in an initial cost of 0, otherwise, a cost of ∞
-- TODO: finish comment nicely once MathJax is working:
-- \(i\)
-- \[ cost(i_c) =
--       \] \(i \elem s_x\), etc...
initializeCostVector :: DiscreteCharacterDecoration d c => d -> SankoffOptimizationDecoration c
initializeCostVector inputDecoration = returnChar
    where
        -- assuming metricity
        inputChar = inputDecoration ^. discreteCharacter
        len       = symbolCount $ inputChar
        range     = [0..len-1]
        costList  = fmap f range
            where
                f i
                    | inputChar `testBit` i = minBound
                    | otherwise             = infinity -- Change this if it's actually Doubles.
        returnChar = extendDiscreteToSankoff inputDecoration costList ([],[]) minBound



-- |
-- Given current node and its children, does actual calculation of new node value
-- for each character state.
--
-- That is, folds over character states, and for each state finds the minimum cost to transition to that
-- state from the characters on each of the left and right children. Stores those mins as a tuple of lists.
-- Likewise, for each state calculates its min (min_left + min_right), as well as the overall lowest min for all states.
--
-- Assumes binary tree.
updateCostVector :: DiscreteCharacterDecoration d c
                 => d
                 -> NonEmpty (SankoffOptimizationDecoration c)
                 -> SankoffOptimizationDecoration c
updateCostVector _parentDecoration (x:|[])                   = x                    -- Shouldn't be possible, but here for completion.
updateCostVector _parentDecoration (leftChild:|rightChild:_) = returnNodeDecoration -- _Should_ be able to amend this to use non-binary children.
    where
        (costVector, dirCostVector, charCost) = foldr findMins initialAccumulator range
        range = [0 .. toEnum $ length (leftChild ^. characterAlphabet) - 1]
        initialAccumulator   = ([], ([],[]), infinity)  -- (min cost per state, (leftMin, rightMin), overall minimum)
        returnNodeDecoration = extendDiscreteToSankoff leftChild costVector dirCostVector $ toWord charCost

        findMins charState (stateMins, (leftMin, rightMin), curMin) = returnVal
             where
                 charMin = if stateMin < curMin
                           then stateMin
                           else curMin
                 stateMin                      = leftChildMin + rightChildMin
                 (leftChildMin, rightChildMin) = calcCostPerState charState leftChild rightChild
                 returnVal                     = (stateMin : stateMins, (leftChildMin : leftMin, rightChildMin : rightMin), charMin)


-- |
-- Takes two decorations in, a child and a parent, and calculates the median character value of the child.
-- For each possible character state this value is based on whether that character state in the child is on
-- one of the min-cost paths from the root to the leaves. It relies on dynamic programming to do so,
-- using the minimum tuple in the parent to determine whether that character state can participate
-- in the final median. Using the left child as a template, the character state is part of the median if,
-- for some state in the parent,
-- parCharState_characterCost_left == childCharState_characterCost + TCM(childCharState, parCharState).
--
-- Used on second, pre-order, pass.
updateDirectionalMins :: EncodableStaticCharacter c -- ERIC: I made this more restrictive to resolve the 'Cannot deduce EncodableStaticCharacter c from Bits c'
                      => SankoffOptimizationDecoration c
                      -> SankoffOptimizationDecoration c
                      -> [ExtendedNatural]
                      -> SankoffOptimizationDecoration c
updateDirectionalMins parentDecoration childDecoration parentMins = childDecoration & discreteCharacter .~ resultMedian
    where
        parentCostVector    = parentDecoration ^. characterCostVector
        parentCharacterCost = parentDecoration ^. characterCost
        emptyMedian         = emptyStatic $ parentDecoration ^. discreteCharacter
        resultMedian        = foldlWithKey' someOtherFunctionThatNeedsName emptyMedian parentMins

        someOtherFunctionThatNeedsName :: EncodableStaticCharacter c => c -> Int -> ExtendedNatural -> c
        someOtherFunctionThatNeedsName acc parentCharState parentCharMin
            | parentCharMin == fromWord parentCharacterCost = foldlWithKey' (buildMedian parentCharState) acc parentCostVector
            | otherwise                                     = acc

        buildMedian :: EncodableStaticCharacter c => Int -> c -> Int -> ExtendedNatural -> c
        buildMedian parentCharState acc childCharState charMin
            | charMin == totalCost childCharState parentCharState = acc `setBit` fromEnum childCharState
            | otherwise                                           = acc

        scm = parentDecoration ^. symbolChangeMatrix

        totalCost i j = fromWord $ parentCharacterCost + tcmCost i j
        tcmCost   i j = scm (toEnum i) (toEnum j)


-- | Take in a single character state as a Word---which represents an unambiguous character state on the parent---
-- and two decorations: the decorations of the two child states.
-- Return the minimum costs of transitioning from each of those two child decorations to the given character.
-- These mins will be saved for use at the next post-order call, to the current parent node's parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we're building medians:
-- the possible character is looped over all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState
    :: EncodedAmbiguityGroupContainer c
    => Word
    -> SankoffOptimizationDecoration c
    -> SankoffOptimizationDecoration c
    -> (ExtendedNatural, ExtendedNatural)
calcCostPerState inputCharState leftChildDec rightChildDec = retVal
    where
        -- Using keys, fold over alphabet states as Ints. The zipped lists will give minimum accumulated costs for
        -- each character state in each child.
        retVal = foldlWithKey' findMins initialAccumulator $ trace (show showableTCM) zippedCostList
        findMins :: (ExtendedNatural, ExtendedNatural) -> Int -> (ExtendedNatural, ExtendedNatural) -> (ExtendedNatural, ExtendedNatural)
        findMins (initLeftMin, initRightMin) childCharState (accumulatedLeftCharCost, accumulatedRightCharCost) = (leftMin, rightMin)
            where
                leftMin             = min curLeftMin  initLeftMin
                rightMin            = min curRightMin initRightMin
                curLeftMin          = trace (unwords ["left : ", show len, show inputCharState, show childCharState, show  accumulatedLeftCharCost, show  leftTransitionCost]) $  leftTransitionCost + accumulatedLeftCharCost
                curRightMin         = trace (unwords ["right: ", show len, show inputCharState, show childCharState, show accumulatedRightCharCost, show rightTransitionCost]) $ rightTransitionCost + accumulatedRightCharCost
                leftTransitionCost  = fromWord . scm inputCharState $ toEnum childCharState
                rightTransitionCost = fromWord . scm inputCharState $ toEnum childCharState

        initialAccumulator = (infinity, infinity)
        zippedCostList     = zip (leftChildDec ^. characterCostVector) (rightChildDec ^. characterCostVector)

        len       = symbolCount $ leftChildDec ^. discreteCharacter
        showableTCM = TCM.generate len g
        g (i,j) = fromEnum $ scm i j
        scm = leftChildDec ^. symbolChangeMatrix
