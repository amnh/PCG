-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Phylogentic Graph types.
--
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bio.PhyloGraphPrime where

--import Bio.PhyloGraphPrime.Component
import Bio.PhyloGraphPrime.Forest
--import Bio.PhyloGraphPrime.Network
--import Bio.PhyloGraphPrime.Tree
import Data.List.NonEmpty            (NonEmpty)
import Data.Semigroup
import Prelude                hiding (lookup)


-- |
-- A solution that contains one or more equally costly forests.
newtype PhylogeneticSolution a
      = PhylogeneticSolution (NonEmpty (PhylogeneticForest a))
      deriving (Semigroup)


{-# INLINE phylogeneticForests #-}
phylogeneticForests :: PhylogeneticSolution a -> NonEmpty (PhylogeneticForest a)
phylogeneticForests (PhylogeneticSolution x) = x
