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

module Bio.Graph.PhylogeneticDAG
  ( PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , PhylogeneticDAGish(..)
  , assignOptimalDynamicCharacterRootEdges
  , assignPunitiveNetworkEdgeCost
  , generateLocalResolutions
  , postorderSequence'
  , preorderFromRooting
  , preorderFromRooting''
  , preorderSequence'
  , preorderSequence''
  , renderSummary
--  , riefiedSolution
--  , riefyForest
  , rootCosts
  , totalEdgeCosts
  , getDotContextWithBaseAndIndex
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


import           Bio.Sequence
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Class
import           Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
import           Bio.Graph.PhylogeneticDAG.Postorder
import           Bio.Graph.PhylogeneticDAG.Preorder
import           Bio.Graph.PhylogeneticDAG.TotalEdgeCost
import           Bio.Graph.ReferenceDAG.Internal
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE


-- HasBlockCost u v w x y z i r
-- |
-- Calculate the root cost for each character block.
{-
rootCosts :: ( Integral e
             , HasCharacterWeight u Double
             , HasCharacterWeight v Double
             , HasCharacterWeight w Double
             , HasCharacterWeight x Double
             , HasCharacterWeight y Double
             , HasCharacterWeight z Double
             , HasCharacterCost u Double
             , HasCharacterCost v e
             , HasCharacterCost w e
             , HasCharacterCost x e
             , HasCharacterCost y e
             , HasCharacterCost z e
             )
-}
rootCosts :: HasBlockCost u v w x y z i r
          => PhylogeneticDAG2 s t u v w x y z -> NonEmpty r
rootCosts (PDAG2 dag) = sequenceCost <$> rootDecs
  where
    roots     = rootRefs dag
    rootDecs  = characterSequence . NE.head . resolutions . nodeDecoration . (references dag !) <$> roots


-- |
-- Map over each node in a pre-order manner.
nodePreorderMap :: (n -> [n'] -> n')
nodePreorderMap = undefined


-- |
-- Map over each edge in a pre-order manner.
edgePreorderMap :: (e -> [e'] -> e')
edgePreorderMap = undefined


-- |
-- Map over each node in a post-order manner.
nodePostorderMap :: (n -> [n'] -> n')
nodePostorderMap = undefined


-- |
-- Map over each edge in a post-order manner.
edgePostorderMap :: (e -> [e'] -> e')
edgePostorderMap = undefined


-- |
-- Fold over each node in a pre-order manner.
nodePreorderFold :: (n -> [a] -> a)
nodePreorderFold = undefined


-- |
-- Fold over each edge in a pre-order manner.
edgePreorderFold :: (e -> [a] -> a)
edgePreorderFold = undefined


-- |
-- Fold over each node in a post-order manner.
nodePostorderFold :: (n -> [a] -> a)
nodePostorderFold = undefined


-- |
-- Fold over each edge in a pre-order manner.
edgePostorderFold :: (e -> [a] -> a)
edgePostorderFold = undefined

