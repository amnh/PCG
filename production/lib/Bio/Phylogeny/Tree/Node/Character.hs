-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A class for a node that has its character information stored
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Character where

import Data.Vector
import Bio.Phylogeny.PhyloCharacter

-- | A character node stores its character info
class CharacterNode a b | a -> b where
    characters :: a -> Vector (PhyloCharacter b)
    setCharacters :: a -> Vector (PhyloCharacter b) -> a