{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Phylogeny.Tree.CharacterAware.Class where

import Data.Vector

class CharacterTree a c | a -> c where
    characters :: a -> Vector c
    setCharacters :: a -> Vector c -> a