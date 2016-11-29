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
-----------------------------------------------------------------------------

data SankoffCharacterDecoration c = SankoffCharacterDecoration
    { minCostVector :: [Word32]
    , dirVectors    :: [c]
    , minCost       :: Word32
    }

-- | Used on the post-order (i.e. first) traversal.
--
sankoffPostOrder :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPostOrder charDecoration childDecorations =
    if isLeaf charDecoration
        then initializeCostVector charDecoration
        else map updateCostVector charDecoration childDecorations

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
--       \] \(i \elem s_x\)
initializeCostVector :: EncodableStaticCharacter c => c -> SankoffCharacterDecoration c
initializeCostVector inputChar = returnChar
    where
        -- assuming metricity and 0 diagonal
        costList = foldMapWithKey f $ getAlphabet inputChar
            where
                f key alphState
                    | key `testBit` inputChar = [minBound :: Word32]
                    | otherwise               = [maxBound :: Word32] -- Change this if it's actually Doubles.
        returnChar = SankoffCharacterDecoration costList [] (minBound :: Word32)

updateCostVector :: EncodableStaticCharacter c => c -> SankoffCharacterDecoration c -> SankoffCharacterDecoration c
updateCostVector curDecoration childDecoration = returnChar
    where
        -- fold over alphabet states, i
            -- fold over alphabet states, j maxBound
                -- if minCost child[i] + tcmCost (i, j) < curMin
                    -- then curMin = minCost child[i] + tcmCost(i, j)
                    -- else curMin = curMin
            -- costList[i] = curMin
            -- fold over alphabet states, j
                -- if curMin == minCost child[i] + tcmCost(i, j)
                    -- then dirVector `setBit` j
                    -- else dirVector `clearBit` j
        -- returnChar = SankoffCharacterDecoration costList dirVector curMin