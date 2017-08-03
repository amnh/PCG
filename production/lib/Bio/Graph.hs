-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph
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

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Bio.Graph
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  ) where

import Bio.Graph.Forest
import Bio.Graph.LeafSet
import           Control.Lens        hiding (Indexable)
import           Data.Foldable
import           Data.GraphViz.Printing hiding ((<>), indent) -- Seriously, why is this redefined?
import           Data.Key
import           Data.List
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Semigroup
import           Prelude             hiding (lookup)
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


instance (HasLeafSet s (LeafSet a), ToXML a, ToXML s) => ToXML (PhylogeneticSolution s) where

    toXML (PhylogeneticSolution forests) = xmlElement "Solution" attrs forestContents
        where
            attrs    = []
            forestContents = [ Right leaves
                             -- , Right graphRepresentation
                             , Right $ collapseElemList "Final_graph" attrs forests
                             ]
            -- (PhylogeneticForest firstForest) = head $ toList soln
            -- (PDAG2 refDag _e _n)    = head $ toList firstForest
            -- (refDag )
            leaves = collapseElemList "Leaf_sets" attrs leafSets

            leafSets = fmap (^. leafSet) <$> forests

            -- TODO: when the dot version is ready, it goes here:
            -- graphRepresentation = xmlElement "Graph" attrs graphRepContents
            -- graphRepContents    = (Right . toXML) <$> toList forests
