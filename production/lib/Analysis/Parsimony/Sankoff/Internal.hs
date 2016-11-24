data SankoffCharacterDecoration c = SankoffCharacterDecoration
    { minCostVector :: [Word32]
    , dirVectors    :: [c]
    , minCost       :: Word32
    }

sankoffPostOrder :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPostOrder charDecoration childDecorations =
    if isLeaf charDecoration
        then initializeCostVector charDecoration
        else map updateCostVector charDecoration childDecorations

sankoffPreOrder  :: ( HasTCM d ((c, c) -> (c, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPreOrder = undefined

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