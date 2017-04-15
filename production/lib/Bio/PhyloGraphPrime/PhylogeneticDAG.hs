------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.PhyloGraphPrime.PhylogeneticDAG
  ( CharacterResult
  , CharacterDAG
  , DecoratedCharacterResult
  , InitialDecorationDAG
  , PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , PhylogeneticDAGish(..)
  , SearchState
  , TopologicalResult
  , UnifiedCharacterSequence
  , UnifiedCharacterBlock
  , UnifiedContinuousCharacter
  , UnifiedDiscreteCharacter
  , UnifiedDynamicCharacter
  , UnRiefiedCharacterDAG
  , assignOptimalDynamicCharacterRootEdges
  , assignPunativeNetworkEdgeCost
  , generateLocalResolutions
  , postorderSequence'
  , rootCosts
  , riefiedSolution
  , riefiedToCharacterDAG
  -- * Mapping over networks
  , edgePreorderMap
  , edgePostorderMap
  , edgePreorderFold
  , edgePostorderFold
  , nodePreorderMap
  , nodePostorderMap
  , nodePreorderFold
  , nodePostorderFold
  ) where


import           Bio.Character.Decoration.Additive
import           Bio.Sequence
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Class
import           Bio.PhyloGraphPrime.PhylogeneticDAG.DynamicCharacterRerooting
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.PhylogeneticDAG.NetworkEdgeQuantification
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Postorder
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Riefication
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Prelude            hiding (zipWith)


rootCosts :: ( Integral e
             , HasCharacterWeight u Double
             , HasCharacterWeight v Double
             , HasCharacterWeight w Double
             , HasCharacterWeight x Double
             , HasCharacterWeight y Double
             , HasCharacterWeight z Double
             , HasCharacterCost u e
             , HasCharacterCost v e
             , HasCharacterCost w Double
             , HasCharacterCost x e
             , HasCharacterCost y e
             , HasCharacterCost z e
             )
          => PhylogeneticDAG2 s t u v w x y z -> NonEmpty Double
rootCosts (PDAG2 dag) = sequenceCost <$> rootDecs
  where
    roots     = rootRefs dag
    rootDecs  = (characterSequence . NE.head . resolutions . nodeDecoration . (references dag !)) <$> roots


nodePreorderMap :: (n -> [n'] -> n')
nodePreorderMap = undefined

edgePreorderMap :: (e -> [e'] -> e')
edgePreorderMap = undefined

nodePostorderMap :: (n -> [n'] -> n')
nodePostorderMap = undefined

edgePostorderMap :: (e -> [e'] -> e')
edgePostorderMap = undefined

nodePreorderFold :: (n -> [a] -> a)
nodePreorderFold = undefined

edgePreorderFold :: (e -> [a] -> a)
edgePreorderFold = undefined

nodePostorderFold :: (n -> [a] -> a)
nodePostorderFold = undefined

edgePostorderFold :: (e -> [a] -> a)
edgePostorderFold = undefined

