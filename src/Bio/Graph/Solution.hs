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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Graph.Solution
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  ) where

import           Bio.Graph.Forest
import           Bio.Graph.LeafSet
import           Bio.Graph.PhylogeneticDAG
import           Bio.Sequence
import           Control.DeepSeq
import           Control.Lens              hiding (Indexable)
import           Data.Foldable
import           Data.GraphViz.Printing    hiding (indent, (<>))
import           Data.GraphViz.Printing    (renderDot, toDot)
import           Data.GraphViz.Types       hiding (attrs)
import           Data.GraphViz.Types.Graph
import           Data.Key
import           Data.List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Semigroup.Foldable
import qualified Data.Text.Lazy            as L
import           GHC.Generics
import           Text.Newick.Class
import           Text.XML


-- |
-- A solution that contains one or more equally costly forests.
newtype PhylogeneticSolution a
      = PhylogeneticSolution (NonEmpty (PhylogeneticForest a))
      deriving (Generic, Semigroup)


-- |
-- Retrieve the non-empty collection of phylogenetic forests from the solution.
{-# INLINE phylogeneticForests #-}
phylogeneticForests :: PhylogeneticSolution a -> NonEmpty (PhylogeneticForest a)
phylogeneticForests (PhylogeneticSolution x) = x


instance (HasLeafSet a b, Semigroup b) => HasLeafSet (PhylogeneticSolution a) b where

    leafSet = lens getter undefined
      where
        getter = foldMap1 (^. leafSet) . phylogeneticForests


instance NFData a => NFData (PhylogeneticSolution a)


instance {-# OVERLAPPABLE #-} PrintDot a => PrintDot (PhylogeneticSolution a) where

    unqtDot       = unqtListToDot . toList . phylogeneticForests

    toDot         =     listToDot . toList . phylogeneticForests

    unqtListToDot = fmap mconcat . traverse unqtDot

    listToDot     = fmap mconcat . traverse   toDot


instance Show n => PrintDot (PhylogeneticSolution (PhylogeneticDAG2 m e n u v w x y z)) where

    unqtDot       = unqtDot . uncurry mkGraph . foldMap1 getSolutionDotContext . phylogeneticForests

    toDot         =   toDot . uncurry mkGraph . foldMap1 getSolutionDotContext . phylogeneticForests

    unqtListToDot = fmap mconcat . traverse unqtDot

    listToDot     = fmap mconcat . traverse   toDot


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


instance ToNewick a => ToNewick (PhylogeneticSolution a) where

    toNewick soln = unlines $ fmap toNewick (toList $ phylogeneticForests soln)


instance
  ( ToXML u
  , ToXML v
  , ToXML w
  , ToXML y
  , ToXML z
  , HasBlockCost u v w x y z
  , Show n
  , Show u
  , Show v
  , Show w
  , Show x
  , Show y
  , Show z
  ) => ToXML (PhylogeneticSolution (PhylogeneticDAG2 m e n u v w x y z)) where

    toXML soln@(PhylogeneticSolution forests) = xmlElement "Solution" attrs forestContents
        where
            attrs          = []
            forestContents = [ Right leaves
                             , Right graphRepresentations
                             , Right characterMetadata
                             , Right $ collapseElemList "Final_graph" attrs forests
                             ]

            leaves   = collapseElemList "Leaf_sets" attrs leafSets
            leafSets = fmap (^. leafSet) <$> forests

            graphRepresentations = xmlElement "Graph_representations" attrs graphContents
            graphContents        = [ Left ("DOT"   , getDOT   soln)
                                   , Left ("Newick", toNewick soln)
                                   ]

            getDOT = L.unpack . renderDot . toDot

            characterMetadata = xmlElement "Character_metadata" attrs metadataContents
            metadataContents  = [Right $ toXML metadataSequence]

            (PDAG2 _ metadataSequence) = NE.head . toNonEmpty $ NE.head forests


getSolutionDotContext
  :: ( FoldableWithKey1 t
     , Functor t
     , Key t ~ Int
     , Show n
     )
  => t (PhylogeneticDAG2 m e n u v w x y z)
  -> ([DotNode GraphID], [DotEdge GraphID])
getSolutionDotContext xs = foldMapWithKey1 g xs
  where
    g = getDotContextWithBaseAndIndex baseValue
    baseValue = maximum $ f <$> xs
    f (PDAG2 dag _) = length dag
