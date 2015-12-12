{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Phylogeny.Tree.CharacterAware.Class where

import Data.Vector

-- | A character aware tree can get the character info used in the tree
class CharacterTree a c | a -> c where
    characters :: a -> Vector c
    setCharacters :: a -> Vector c -> a