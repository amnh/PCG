{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Character where

import Data.Vector
import Bio.Phylogeny.PhyloCharacter

class CharacterNode a b | a -> b where
    characters :: a -> Vector (PhyloCharacter b)
    setCharacters :: a -> Vector (PhyloCharacter b) -> a