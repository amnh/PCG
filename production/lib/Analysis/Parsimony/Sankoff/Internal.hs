-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Sankoff
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

import Control.Lens
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable

data SankoffCharacterDecoration c = SankoffCharacterDecoration
    { minCostVector        :: [Word32]           -- overall min for each character state
    , directionalMinVector :: [(Word32, Word32)] -- for a character state, its min from the left child and its min from the right
    , minCost              :: Word32
    }

-- | Used on the post-order (i.e. first) traversal.
sankoffPostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
sankoffPostOrder charDecoration childDecorations =
    if isLeaf charDecoration
        then initializeCostVector charDecoration
        else updateCostVector charDecoration childDecorations

-- | Used on the pre-order (i.e. second) traversal.
sankoffPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (SankoffCharacterDecoration c -> [d'] -> d')
sankoffPreOrder charDecoration childDecorations =
    if isRoot charDecoration
        then initializeDirVector charDecoration
        else updateDirVector charDecoration childDecorations

-- | Before post-order can actually occur, must initialize leaf vectors with values as such:
-- Given \(n\) character states, for a given character \(i_c\) on leaf \(i\), there are \(2^n - 1)
-- possible characters, including ambiguous characters. For extant character states \(i_c_x\) on
-- the leaf, and for each possible character state, \(i\)
-- \[ cost(i_c) =
--       \] \(i \elem s_x\), etc...
-- TODO: finish above comment once MathJax is working
initializeCostVector :: DiscreteCharacterDecoration d c => c -> SankoffCharacterDecoration c
initializeCostVector inputDecoration = returnChar
    where
        -- assuming metricity and 0 diagonal
        costList = foldMapWithKey f $ inputDecoration ^. characterAlphabet
            where
                f key alphState
                    | key `testBit` inputChar = [minBound :: Word32]
                    | otherwise               = [maxBound :: Word32] -- Change this if it's actually Doubles.
                    where inputChar = (inputDecoration ^. discreteCharacter)
        returnChar = SankoffCharacterDecoration costList [] (minBound :: Word32)

-- | Given current node and its children, does actual calculation of new node value:
-- [overall minimum cost, [minimum cost for character state], [direction arrows]],
-- for each character state, for each character on current node. Does not assume binary tree.

-- TODO: Think we can eliminate the first argument here:
updateCostVector :: EncodableStaticCharacter c => c -> [SankoffCharacterDecoration c] -> SankoffCharacterDecoration c
updateCostVector _                 []                       = undefined
updateCostVector _                 (x:[])                   = undefined
updateCostVector curNodeDecoration (leftChild:rightChild:_) = returnNodeDecoration -- _Should_ be able to amend this to use non-binary children.
    where
        (charCost, costVector, dirCostVector) =
            foldlWithKey' (\(charMin, costs, diretionalMins) charState _ -> (charMin, costs : stateMin, diretionalMins : childMins)
                       where
                           charMin = if stateMin < charMin
                               then charMin
                               else stateMin
                           stateMin  = leftChildMin + rightChildMin
                           childMins =
                               calcCostPerState charState (leftChild ^. discreteCharacter) (rightChild ^. discreteCharacter)
                   ) (maxBound :: Word32, [], []) (curNodeDecoration ^. characterAlphabet)
        returnChar = SankoffCharacterDecoration costVector [] charCost

initializeDirVector :: SankoffCharacterDecoration c -> SankoffCharacterDecoration c
initializeDirVector curDecoration = returnChar
    where
        median = foldlWithKey' buildMedian startMedian $ curDecoration ^. minCostVector
        buildMedian acc key charMin
            | charMin == curDecoration ^. minCost = acc `setBit` key
            | otherwise                           = acc
        startMedian = (curDecoration ^. discreteCharacter) `xor` (curDecoration ^. discreteCharacter)
        returnChar = SankoffCharacterDecoration (curDecoration ^. minCostVector) median (curDecoration ^. minCost) -- TODO: this is not where median goes. Fix it.

updateDirVector :: SankoffCharacterDecoration c -> SankoffCharacterDecoration c -> SankoffCharacterDecoration c
updateDirVector parentDecoration childDecoration = returnChar
    where
        median = foldlWithKey' buildMedian startMedian $ zip (childDecoration ^. minCostVector) (parentDecoration ^. directionalMinVector)
        buildMedian acc key charMin
            | charMin == curDecoration ^. minCost = acc `setBit` key
            | otherwise                           = acc
        startMedian = (childDecoration ^. discreteCharacter) `xor` (childDecoration ^. discreteCharacter)
        returnChar  = SankoffCharacterDecoration (curDecoration ^. minCostVector) median (curDecoration ^. minCost) -- TODO: this is not where median goes. Fix it.

-- | Take in a single character state as an Int, which represents an possible unambiguous character state on the parent,
-- and two decorations: the decorations of the two child states.
-- Return the minimum costs of transitioning from each of those two child decorations to the given character.
-- These mins will be saved for use at the next post-order call, to the current parent node's parent.
--
-- Note: We can throw away the medians that come back from the tcm here because we're building medians: the possible character is looped over
-- all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState :: Int -> DiscreteCharacterDecoration DiscreteCharacterDecoration -> (Int, Int)
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

calcDirPerState :: Int -> Int -> Int -> DiscreteCharacterDecoration DiscreteCharacterDecoration -> (Bool, Bool)
calcDirPerState inputCharState leftChildCharMin rightChildMin leftChildDec rightChildDec =
    foldlWithKey' (\(leftCharDirList, rightCharDirList) key (leftChildAccumCost, rightChildAccumCost) ->
                       (leftCharDirList : leftCharDir, rightCharDirList : rightCharDir)
                       where
                           leftCharDir         = leftTransitionCost  + leftChildAccumCost  == leftChildCharMin
                           rightCharDir        = rightTransitionCost + rightChildAccumCost == rightChildMin
                           leftTransitionCost  = (leftChildDec  ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState
                           rightTransitionCost = (rightChildDec ^. characterSymbolTransitionCostMatrixGenerator) inputCharState childCharState

                  ) ([],[]) $ zip (leftChildDec ^. minCostVector) (rightChildDec ^. minCostVector)




