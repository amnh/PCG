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
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Graph.Solution
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  , extractSolution
  ) where

import           Bio.Graph.Forest
import           Bio.Graph.LeafSet
import           Bio.Graph.PhylogeneticDAG
import           Bio.Sequence
import           Control.DeepSeq
import           Control.Lens              as Lens hiding (Indexable)
import           Data.Foldable
import           Data.GraphViz.Printing    hiding (indent)
import           Data.GraphViz.Printing    (renderDot, toDot)
import           Data.GraphViz.Types       hiding (attrs)
import           Data.GraphViz.Types.Graph
import           Data.Key
import           Data.List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Semigroup.Foldable
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           GHC.Generics
import           Text.Newick.Class
import           Text.XML
import           TextShow                  (TextShow (showb, showtl), fromLazyText)
import           Type.Reflection           (Typeable)


-- |
-- A solution that contains one or more equally costly forests.
newtype PhylogeneticSolution a
      = PhylogeneticSolution {getPhylogeneticSolution :: NonEmpty (PhylogeneticForest a)}
      deriving (Generic, Semigroup, Typeable)


-- |
-- Retrieve the non-empty collection of phylogenetic forests from the solution.
{-# INLINE phylogeneticForests #-}
phylogeneticForests :: PhylogeneticSolution a -> NonEmpty (PhylogeneticForest a)
phylogeneticForests (PhylogeneticSolution x) = x

-- |
-- Extract a solution without loss of generality.
{-# INLINE extractSolution #-}
extractSolution :: PhylogeneticSolution a -> a
extractSolution = NE.head . getPhylogeneticForest . NE.head . getPhylogeneticSolution


instance (HasLeafSet a b, Semigroup b) => HasLeafSet (PhylogeneticSolution a) b where

    leafSet = Lens.to getter
      where
        getter = foldMap1 (^. leafSet) . phylogeneticForests


instance NFData a => NFData (PhylogeneticSolution a)


instance {-# OVERLAPPABLE #-} PrintDot a => PrintDot (PhylogeneticSolution a) where

    unqtDot       = unqtListToDot . toList . phylogeneticForests

    toDot         =     listToDot . toList . phylogeneticForests

    unqtListToDot = fmap mconcat . traverse unqtDot

    listToDot     = fmap mconcat . traverse   toDot


instance TextShow n => PrintDot (PhylogeneticSolution (PhylogeneticDAG m e n u v w x y z)) where

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

instance TextShow a => TextShow (PhylogeneticSolution a) where

    showb = fromLazyText
          . ("Solution:\n\n" <>)
          . indent
          . renderForests
          . fmap renderForest
          . phylogeneticForests
      where
        indent       = TL.intercalate  "\n" . fmap ("  " <>) . TL.lines
        renderForest = indent . foldMapWithKey f
          where
            f k e = fold
                [ "Component #"
                , showtl k
                , ":\n\n"
                , indent $ showtl e
                , "\n"
                ]
        renderForests = indent . foldMapWithKey f
          where
            f k e = fold
                [ "Forest #"
                , showtl k
                , ":\n\n"
                , indent e
                , "\n"
                ]


instance ToNewick a => ToNewick (PhylogeneticSolution a) where

    toNewick soln = T.unlines $ fmap toNewick (toList $ phylogeneticForests soln)


instance
  ( HasBlockCost u v w x y z
  , TextShow n
  , TextShow u
  , TextShow v
  , TextShow w
  , TextShow x
  , TextShow y
  , TextShow z
  , ToXML u
  , ToXML v
  , ToXML w
  , ToXML y
  , ToXML z
  ) => ToXML (PhylogeneticSolution (PhylogeneticDAG m e n u v w x y z)) where

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
                                   , Left ("Newick", T.unpack $ toNewick soln)
                                   ]

            getDOT = TL.unpack . renderDot . toDot

            characterMetadata = xmlElement "Character_metadata" attrs metadataContents
            metadataContents  = [Right $ toXML metadataSequence]

            (PDAG2 _ metadataSequence) = NE.head . toNonEmpty $ NE.head forests


getSolutionDotContext
  :: ( FoldableWithKey1 t
     , Functor t
     , Key t ~ Int
     , TextShow n
     )
  => t (PhylogeneticDAG m e n u v w x y z)
  -> ([DotNode GraphID], [DotEdge GraphID])
getSolutionDotContext xs = foldMapWithKey1 g xs
  where
    g = getDotContextWithBaseAndIndex baseValue
    baseValue = maximum $ f <$> xs
    f (PDAG2 dag _) = length dag
