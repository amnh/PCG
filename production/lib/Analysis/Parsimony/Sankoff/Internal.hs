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
-----------------------------------------------------------------------------

import Control.Lens
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable

data SankoffCharacterDecoration c = SankoffCharacterDecoration
    { minCostVector :: [Word32]
    , dirVector     :: [(Bool, Bool)] -- for a character state, (from left, from right)
    , minCost       :: Word32
    }

-- | Used on the post-order (i.e. first) traversal.
--
sankoffPostOrder :: ( EncodableStaticCharacter c, DiscreteCharacterDecoration d c ) => (d -> [d'] -> d')
sankoffPostOrder charDecoration childDecorations =
    if isLeaf charDecoration
        then initializeCostVector charDecoration
        else updateCostVector charDecoration childDecorations -- childDecorations needs to be a tuple

-- | Used on the pre-order (i.e. second) traversal.
sankoffPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPreOrder = undefined

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
updateCostVector :: EncodableStaticCharacter c => c -> (SankoffCharacterDecoration c, SankoffCharacterDecoration c) -> SankoffCharacterDecoration c
updateCostVector curNodeDecoration []      = undefined
updateCostVector curNodeDecoration (x:[])  = undefined
updateCostVector curNodeDecoration (x:y:_) = returnNodeDecoration
    where
{-        -- fold over characters
        costTuple = foldl (\x acc -> (fst acc : overallMin, snd acc : perCharStateMin)
                            where
                                (overallMin, perCharStateMin) = generateSingleCharDec (getAlphabet curNodeDecoration) childCharTup
                                dirVector = generateDirVector overallMin perCharStateMin childCharTup
                          ) ([],[]) $ zipWith (\a b -> (getChars a, getChars b)) leftChildDec rightChildDec
-}
        -- There's an overall min for that character, charMin, as well as a list of mins corresponding to the minimum cost for each character state, [stateMin]
        (minCost, costVector) = foldl (\charState (curDecoration ^. discreteCharacter, minC, costs) -> (charMin, costs : stateMin)
                                          where
                                              (charMin, stateMin) = calcCostPerState (curDecoration ^. discreteCharacter) minC charState
                                      ) (maxBound :: Word32, []) $ curDecoration ^. characterAlphabet -- fold over alphabet states
                            where
                                finalMin =
                                    -- if minCost child[i] + tcmCost (i, j) < curMin
                                    if newMin < curMin
                                        then newMin
                                        else curMin
                                newMin = minCost childDecoration + tcm[alphIdxI, alphIdxJ]
                    costList : curMin
        dirList = costList[i] = curMin
            foldl
            -- fold over alphabet states, j
                -- if curMin == minCost child[i] + tcmCost(i, j)
                    -- then dirVector `setBit` j
                    -- else dirVector `clearBit` j
        returnChar = SankoffCharacterDecoration minCost costVector dirVector

-- | Take in a single character state, represented by an int (which bit is on) and two decorations (the decorations of the two child states).
-- Return the total cost of transitioning from those two child decorations to the given character.
--
-- We can throw away the medians here because we're building medians: the possible character is looped over
-- all available characters, and there's an outer loop which sends in each possible character.
calcCostPerState :: Int -> [DiscreteCharacterDecoration] -> Word32
calcCostPerState children charState = cost
    -- foldlWithKey (\key (cost, ) value ->  ) child ^. minCostVector
    foldl (\child cost -> transitionCost + child ^. minCost
            where
                (tansitionCost, _) = (child ^. characterTCM) (child ^. discreteCharacter) charState
           ) 0 children

        leftCost = (median, cost) = (leftChild ^. characterTCM) (leftChild ^. discreteCharacter) + leftChild ^. minCost

-- | Takes in two characters, then folds over possible character states, calculating the minimum cost of having that character state
-- as a member of the possibly ambiguous character in the median
generateSingleCharDec :: Alphabet -> (AlphabetChar, AlphabetChar) -> (Word32, [Word32])
generateSingleCharDec (curDecoration, leftChildDec, rightChildDec) =
    calcCostPerState charState [maxBound :: Word32]