{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.Tree.Node.Character where

import Data.Vector
import Data.PhyloCharacter

class CharacterNode a b | a -> b where
    characters :: a -> Vector (PhyloCharacter b)
    setCharacters :: a -> Vector (PhyloCharacter b) -> a