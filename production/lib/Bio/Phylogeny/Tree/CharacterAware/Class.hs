-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for a character aware tree that knows the info for each character
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Phylogeny.Tree.CharacterAware.Class where

import Data.Vector
import Bio.Phylogeny.PhyloCharacter

-- | A character aware tree can get the character info used in the tree
class CharacterTree a s | a -> s where
    characters :: a -> Vector (PhyloCharacter s)
    setCharacters :: a -> Vector (PhyloCharacter s) -> a