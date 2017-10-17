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
-- {-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module Bio.Graph
  ( CharacterResult
  , CharacterDAG
  , DecoratedCharacterResult
  , FinalDecorationDAG
  , GraphState
  , PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , PhylogeneticDAGish(..)
  , PhylogeneticForest(..)
  , PhylogeneticSolution(..)
  , PostOrderDecorationDAG
  , SearchState
  , TopologicalResult
  , UnifiedCharacterSequence
  , UnifiedCharacterBlock
  , UnifiedContinuousCharacter
  , UnifiedDiscreteCharacter
  , UnifiedDynamicCharacter
  , UnReifiedCharacterDAG
  , assignOptimalDynamicCharacterRootEdges
  , assignPunitiveNetworkEdgeCost
  , generateLocalResolutions
  , postorderSequence'
  , preorderFromRooting
  , preorderSequence'
  , preorderSequence''
  , renderSummary
  , reifiedSolution
--  , reifiedToCharacterDAG
  , rootCosts
  , phylogeneticForests
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


-- import           Bio.Sequence
import           Bio.Graph.Constructions
-- import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG
-- import           Bio.Graph.PhylogeneticDAG.Class
-- import           Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
-- import           Bio.Graph.PhylogeneticDAG.Internal
-- import           Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
-- import           Bio.Graph.PhylogeneticDAG.Postorder
-- import           Bio.Graph.PhylogeneticDAG.Preorder
import           Bio.Graph.PhylogeneticDAG.Reification
-- import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Graph.Solution
-- import           Data.Key
-- import           Data.List.NonEmpty        (NonEmpty)
-- import qualified Data.List.NonEmpty as NE
import           Prelude            hiding (zipWith)


{-
import           Bio.Graph.Forest
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Control.Lens           hiding (Indexable)
import           Data.Foldable
import           Data.GraphViz.Printing hiding ((<>), indent) -- Seriously, why is this redefined?
import           Data.Key
import           Data.List
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Prelude                hiding (lookup)
import           Text.XML.Custom


-- |
-- A solution that contains one or more equally costly forests.
newtype PhylogeneticSolution a
      = PhylogeneticSolution (NonEmpty (PhylogeneticForest a))
      deriving (Semigroup)


-- |
-- Retrieve the non-empty collection of phylogenetic forests from the solution.
{-# INLINE phylogeneticForests #-}
phylogeneticForests :: PhylogeneticSolution a -> NonEmpty (PhylogeneticForest a)
phylogeneticForests (PhylogeneticSolution x) = x


instance PrintDot a => PrintDot (PhylogeneticSolution a) where

    unqtDot       = unqtListToDot . toList . phylogeneticForests

    toDot         = listToDot . toList . phylogeneticForests

    unqtListToDot = fmap mconcat . sequenceA . fmap unqtDot

    listToDot     = fmap mconcat . sequenceA . fmap toDot


instance Show a => Show (PhylogeneticSolution a) where

    show = ("Solution:\n\n" <>) . indent . renderForests . fmap renderForest . phylogeneticForests
      where
        indent = intercalate "\n" . fmap ("  "<>) . lines
        renderForest = indent . foldMapWithKey f
          where
            f k e = mconcat
                [ "Component #"
                , show k
                , ":\n\n"
                , indent $ show e
                , "\n"
                ]
        renderForests = indent . foldMapWithKey f
          where
            f k e = mconcat
                [ "Forest #"
                , show k
                , ":\n\n"
                , indent e
                , "\n"
                ]


instance (ToXML s) => ToXML (PhylogeneticSolution s) where

    toXML (PhylogeneticSolution soln) = xmlElement "Solution" attrs contents
        where
            attrs    = []
            contents = [-- Right leafSetElement
                       -- , Right graphRepresentation
                        Right $ collapseElemList "Final graph" attrs soln
                       ]
            -- (PhylogeneticForest firstForest) = head $ toList soln
            -- (PDAG2 refDag _e _n)    = head $ toList firstForest
            -- (refDag )
--            leafSetElement = collapseElemList "Leaf Set" attrs leaves

--            leaves = fmap nodeDecorationDatum2 $ soln ^. leafSet


instance (HasLeafSet s a, Semigroup a) => HasLeafSet (PhylogeneticSolution s) a where

    leafSet = lens getter setter
      where
        setter e _ = id e
        getter = foldMap1 (foldMap1 (^. leafSet)) . phylogeneticForests
-}
