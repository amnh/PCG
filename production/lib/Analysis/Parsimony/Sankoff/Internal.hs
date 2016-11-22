

sankoffPostOrder :: ( HasTCM d ((StaticCharacter, StaticCharacter) -> (StaticCharacter, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPostOrder = undefined

sankoffPreOrder  :: ( HasTCM d ((StaticCharacter, StaticCharacter) -> (StaticCharacter, Double))
                    , HasStaticCharacter d c
                    , EncodableStaticCharacter c
                    ) => (d -> [d'] -> d')
sankoffPreOrder curNode childNodes =
    if isLeaf curNode
        then map
