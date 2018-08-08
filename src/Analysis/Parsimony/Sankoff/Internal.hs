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
-- This only works on static characters, and due to the traversal only one
-- character will be received at a time.
--
-- Goloboff’s algorithm relies on computing the “extra cost” for each non-
-- optimal state assignment for each node. There’s a preliminary extra
-- cost, which is the difference between the assignment cost for this state
-- on this node, and a final extra cost, which is the total extra cost
-- when recursing over the whole tree that this state assignment implies.
-- This involves, then, computing extra costs even for states that are non-
-- optimal.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Sankoff.Internal where


import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Metric
import           Bio.Character.Encodable
import           Bio.Metadata
import           Control.Lens
import           Data.Bits
import           Data.Key
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import           Data.Word
import           Numeric.Extended.Natural
import           Prelude                           hiding (zip)


-- |
-- Used on the post-order (i.e. first) traversal.
sankoffPostOrder
  :: DiscreteCharacterDecoration d c
  => DiscreteWithTCMCharacterMetadataDec c
  -> d
  -> [SankoffOptimizationDecoration c]
  ->  SankoffOptimizationDecoration c
sankoffPostOrder meta charDecoration xs =
  case xs of
    []   -> initializeCostVector charDecoration -- is a leaf
    y:ys -> updateCostVector meta charDecoration (y:|ys)


-- |
-- Used on the pre-order (i.e. second) traversal.
--
-- Either calls `initializeDirVector` on root or `updateDirectionalMins`.
-- Needs to determine which child it’s updating, then sends the appropriate
-- minlist to `updateDirectionalMins`.
sankoffPreOrder
  :: EncodableStaticCharacter c
  => SankoffOptimizationDecoration c
  -> [(Word, SankoffOptimizationDecoration c)]
  -> SankoffOptimizationDecoration c
sankoffPreOrder childDecoration [] = newDecoration       -- is a root
  where
    childMins     = childDecoration ^. characterCostVector
    overallMin    = childDecoration ^. characterCost
    emptyMedian   = emptyStatic $ childDecoration ^. discreteCharacter
    newChar       = foldlWithKey' setState emptyMedian childMins
    newDecoration = childDecoration & discreteCharacter .~ newChar

    setState acc pos childMin
      | unsafeToFinite childMin == overallMin = acc `setBit` pos
      | otherwise                             = acc

sankoffPreOrder childDecoration ((whichChild, parentDecoration):_) = resultDecoration $   -- is either internal node or leaf
    case whichChild of
      0 -> fst
      _ -> snd
  where
    resultDecoration f = updateDirectionalMins parentDecoration childDecoration $ f (parentDecoration ^. minStateTuple)


-- |
-- Before post-order can actually occur, must initialize leaf vectors with values
-- as such:
--
-- Given \(n\) character states, for a given character \(i_c\) on leaf \(i\),
-- there are \(2^n - 1\) possible characters, including ambiguous characters. For
-- extant character states \(i_{c_x}\) on the leaf, and for each possible character
-- state, if that character state is extant on the leaf, give it an initial cost
-- of 0, otherwise, ∞.

-- TODO: What’s this? \(i\)
-- \[ cost(i_c) =
--       \] \(i \exists s_x\), etc...
initializeCostVector :: DiscreteCharacterDecoration d c => d -> SankoffOptimizationDecoration c
initializeCostVector inputDecoration =
    extendDiscreteToSankoff
      inputDecoration
      costList
      costList
      []
      []
      ([], [])
      minBound
      inputChar
      True
  where
    -- assuming metricity
    inputChar = inputDecoration ^. discreteCharacter
    range     = [0..5]
    costList  = fmap f range
      where
        f i
          | inputChar `testBit` i = minBound
          | otherwise             = infinity -- Change this if it’s actually Doubles.


-- |
-- Used on the post-order (i.e. first) traversal, moving from leaves to root.
--
-- Given current node and its children, does actual calculation of new node values
-- for each character state:
--
--    * \(\forall a, minCosts_a = \forall b, \(min (transitionCost(a, b) + leftChild_{minCost(b)} + rightChild_{minCost(b)}\).
--    (This is a tuple of lists), where
--    * \(\forall a, minCost(a) = min(minCosts_a)\).
--    * \(nodeMin = min(minCost)\)
--
-- In order to run Goloboff’s Sankoff traversal optimizations, computes:
--
--    * \(\forall a, preliminaryExtraCost(a) = \(minCost(a) - nodeMin\)
--    * \(\beta(a, b) = min_{a,b} (transitionCost (a, b) + preliminaryExtraCost(b))\)
--
-- This node is not a leaf node. Assumes binary tree.

-- TODO: Do I need this, or is it redundant?
-- Likewise, for each state, calculates its min state \(min_{left} + min_{right}\)
-- from the characters on each of the left and right children. Stores those mins as a tuple of lists.
--
--
updateCostVector
  :: DiscreteCharacterDecoration d c
  => DiscreteWithTCMCharacterMetadataDec c
  -> d
  -> NonEmpty (SankoffOptimizationDecoration c)
  -> SankoffOptimizationDecoration c
updateCostVector _meta _parentDecoration (x:|[])                        = x                    -- Shouldn't be possible, but here for completion.
updateCostVector meta _parentDecoration (leftChildDec:|rightChildDec:_) = returnNodeDecoration -- May? be able to amend this to use non-binary children.
  where
    (cs, ds, minTransCost) = foldr findMins initialAccumulator range   -- Sorry abut these shitty variable names. It was to shorten
                                                                       -- the 'extendDiscreteToSankoff' call.
                                                                       -- cs = min costs per state
                                                                       -- ds = (left child min states, right child min states)
    range                = [0 .. numAlphStates]
    numAlphStates        = symbolCount charWLOG
    preliminaryMins      = foldr         computeExtraMin [] cs
    bs                   = foldrWithKey' computeBetas    [] range      -- bs  = betas
    omc                  = unsafeToFinite minTransCost                 -- omc = overall min cost (min for all states)
    scm                  = meta ^. symbolChangeMatrix

    initialAccumulator   = ([], ([],[]), infinity)                   -- (min cost per state, (leftMin, rightMin), overall minimum)
    returnNodeDecoration = extendDiscreteToSankoff leftChildDec cs preliminaryMins [] bs ds omc emptyMedian False
    charWLOG             = leftChildDec ^. discreteCharacter
    emptyMedian          = emptyStatic charWLOG

    computeExtraMin thisCost acc = (thisCost - minTransCost) : acc

    computeBetas charState _childCharState acc = retVals
      where
        retVal  = minimum [ prelimMin + fromFinite (scm (toEnum charState) otherState)
                          | (otherState, prelimMin) <- zip range preliminaryMins
                          ]
        retVals = retVal : acc

    findMins
      :: Word
      -> ([ExtendedNatural], ([StateContributionList], [StateContributionList]), ExtendedNatural)
      -> ([ExtendedNatural], ([StateContributionList], [StateContributionList]), ExtendedNatural)
    findMins charState (stateMins, (accumulatedLeftChildStates, accumulatedRightChildStates), accMin) =
        ( stateMin : stateMins
        , (leftChildRetStates : accumulatedLeftChildStates, rightChildRetStates : accumulatedRightChildStates)
        , curMin
        )
      where
        curMin
          | stateMin < accMin = stateMin
          | otherwise         = accMin
        stateMin  = leftMin + rightMin
        ((leftMin, leftChildRetStates), (rightMin, rightChildRetStates)) = calcCostPerState scm charState leftChildDec rightChildDec


-- |
-- Takes two decorations in, a child and a parent, and calculates the median
-- character value of the child. For each possible character state this value is
-- based on whether that character state in the child is on one of the min-cost
-- paths from the root to the leaves. It relies on dynamic programming to do so,
-- using the minimum tuple in the parent to determine whether that character
-- state can participate in the final median. Without loss of generality, for the left child,
-- the character state \(a\) is part of the median if, for some character \(b\) in the child,
--
-- \[cost(a_{parent_{leftChild}}) == cost(b_{child}) + transitionCost(a, b).\]
--
-- Used on second, pre-order, pass.
updateDirectionalMins
  :: EncodableStaticCharacter c -- TODO: I made this more restrictive to resolve the 'Cannot deduce
                                -- EncodableStaticCharacter c from Bits c'
  => SankoffOptimizationDecoration c
  -> SankoffOptimizationDecoration c
  -> [StateContributionList]
  -> SankoffOptimizationDecoration c
updateDirectionalMins parentDecoration childDecoration childStateMinsFromParent = childDecoration & discreteCharacter .~ resultMedian
  where
    parentFinalMedian = parentDecoration ^. discreteCharacter
    emptyMedian       = emptyStatic $ parentDecoration ^. discreteCharacter
    resultMedian
      -- discreteChar doesn't change
      | childDecoration ^. isLeaf = childDecoration ^. discreteCharacter
      -- need to create new bit vector
      | otherwise                 = foldlWithKey' determineWhetherToIncludeState emptyMedian childStateMinsFromParent

    -- If this character state in the parent is one of the low-cost states, then add all states in child that can contribute
    -- to this parent state.
    determineWhetherToIncludeState :: EncodableStaticCharacter c => c -> Int-> StateContributionList -> c
    determineWhetherToIncludeState acc parentCharState childStateMinList
      | parentFinalMedian `testBit` parentCharState = foldl setState acc childStateMinList
      | otherwise                                   = acc

    setState :: EncodableStaticCharacter c => c -> Word -> c
    setState newMedian charState = newMedian `setBit` (fromIntegral charState :: Int)


-- |
-- Takes in a single character state as a `Word`---which represents an unambiguous character state on the parent---
-- and two decorations, the decorations of the two child states.
-- Returns the minimum costs of transitioning from the input character to each of those two child decorations.
-- These mins will be saved for use at the next post-order call, to the current parent node’s parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we’re building medians:
-- the possible character is looped over all available characters, and there’s an outer loop which sends in each possible character.
calcCostPerState
  :: (Word -> Word -> Word)
  -> Word
  -> SankoffOptimizationDecoration c
  -> SankoffOptimizationDecoration c
  -> ((ExtendedNatural, StateContributionList), (ExtendedNatural, StateContributionList))
calcCostPerState scm parentCharState leftChildDec rightChildDec = retVal
  where
    -- Using keys, fold over child alphabet states as Ints. Look up the transition from parentCharState to that alphabet state.
    -- For each child alphabet state the zipped cost list has the minimum accumulated costs for that character state in each child.
    -- calcCostPerState is called inside another loop, so here we loop only over the child states.
    retVal = foldlWithKey' findMins initialAccumulator zippedCostList
    initialAccumulator = ((infinity,[]), (infinity,[]))
    zippedCostList     = zip (leftChildDec ^. characterCostVector) (rightChildDec ^. characterCostVector)

    findMins
      :: ((ExtendedNatural, [Word]), (ExtendedNatural, [Word]))
      -> Int
      -> (ExtendedNatural, ExtendedNatural)
      -> ((ExtendedNatural, [Word]), (ExtendedNatural, [Word]))
    findMins ((accumulatedLeftCharCost, originalLeftStates), (accumulatedRightCharCost, originalRightStates)) childCharState (leftMinFromVector, rightMinFromVector) =
        ((leftMin, minLeftStates), (rightMin, minRightStates))
      where
        (leftMin, minLeftStates)
          | curLeftMin  <  accumulatedLeftCharCost  = (curLeftMin              , [toEnum childCharState]                    )
          | curLeftMin  == accumulatedLeftCharCost  = (accumulatedLeftCharCost , toEnum childCharState : originalLeftStates )
          | otherwise                               = (accumulatedLeftCharCost , originalLeftStates                         )

        (rightMin, minRightStates)
          | curRightMin <  accumulatedRightCharCost = (curRightMin             , [toEnum childCharState]                    )
          | curRightMin == accumulatedRightCharCost = (accumulatedRightCharCost, toEnum childCharState : originalRightStates)
          | otherwise                               = (accumulatedRightCharCost, originalRightStates                        )

        curLeftMin      = transitionCost + leftMinFromVector
        curRightMin     = transitionCost + rightMinFromVector
        transitionCost  = fromFinite . scm parentCharState $ toEnum childCharState
