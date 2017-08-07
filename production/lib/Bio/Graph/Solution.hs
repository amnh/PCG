-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Solution
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Bio.Graph.Solution
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  ) where

import           Bio.Graph.Forest
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Lens           hiding (Indexable)
import           Data.Foldable
import           Data.GraphViz.Printing hiding ((<>), indent) -- Seriously, why is this redefined?
import           Data.GraphViz.Printing        (renderDot, toDot)
import           Data.Key
import           Data.List
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty     as NE
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Text.Lazy         as L
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
        indent       = intercalate "\n" . fmap ("  " <>) . lines
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


instance (HasLeafSet s (LeafSet a), PrintDot s, ToXML a, ToXML s) => ToXML (PhylogeneticSolution s) where

    toXML soln@(PhylogeneticSolution forests) = xmlElement "Solution" attrs forestContents
        where
            attrs          = []
            forestContents = [ Right leaves
                             , Right graphRepresentations
                             -- , Right characterMetadata
                             , Right $ collapseElemList "Final_graph" attrs forests
                             ]

            leaves   = collapseElemList "Leaf_sets" attrs leafSets
            leafSets = fmap (^. leafSet) <$> forests

            graphRepresentations = xmlElement "Graph_representations" attrs graphContents
            graphContents        = [ Left ("DOT", getDOT soln)
                                   --, Right graphASCII
                                   ]
            -- graphASCII           = xmlElement "Graphical" attrs graphASCIIContents
            -- graphASCIIContents   = (Right . toXML) <$> toList forests

            getDOT :: PrintDot a => PhylogeneticSolution a -> String
            getDOT = L.unpack . renderDot . toDot

            -- characterMetadata = xmlElement "Character_metadata" attrs metadataContents
            -- metadataContents  = [Right $ toXML arbitraryCharSeq]

            -- arbitraryCharSeq  = characterSequence . NE.head . resolutions . nodeDecoration $ arbitraryNode
            --     where
            --         arbitraryNode = references arbitraryRefDAG ! arbitraryRootRef
            --         (PDAG2 arbitraryRefDAG) = NE.head . toNonEmpty $ NE.head forests
            --         arbitraryRootRef        = NE.head $ rootRefs arbitraryRefDAG


