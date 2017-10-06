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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module Bio.Graph.Solution
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  ) where

import           Bio.Graph.Forest
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
-- import           Bio.Metadata.General
import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
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
import           Data.TCM                      (generate)
import qualified Data.Text.Lazy         as L
import           Prelude                hiding (lookup)
import           Text.Newick.Class
import           Text.XML


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


instance (HasLeafSet a b, Semigroup b) => HasLeafSet (PhylogeneticSolution a) b where

    leafSet = lens getter undefined
      where
        getter = foldMap1 (^. leafSet) . phylogeneticForests


instance PrintDot a => PrintDot (PhylogeneticSolution a) where

    unqtDot       = unqtListToDot . toList . phylogeneticForests

    toDot         =     listToDot . toList . phylogeneticForests

    unqtListToDot = fmap mconcat . sequenceA . fmap unqtDot

    listToDot     = fmap mconcat . sequenceA . fmap   toDot


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
  ( -- Show n
--   Show (f String)
--  , ToXML (f String)
    ToXML u
  , ToXML v
  , ToXML w
  , ToXML x
  , ToXML y
  , ToXML z
  , GeneralCharacterMetadata  u
  , DiscreteCharacterMetadata v
  , DiscreteCharacterMetadata w
  , DiscreteCharacterMetadata x
  , DiscreteCharacterMetadata y
  , DiscreteCharacterMetadata z
  , Applicative f
  , Foldable f
  -- , HasCharacterAlphabet x f
  -- , HasCharacterAlphabet y f
  -- , HasCharacterAlphabet z f
  , HasSymbolChangeMatrix x (Word -> Word -> Word)
  , HasSymbolChangeMatrix y (Word -> Word -> Word)
  , HasSymbolChangeMatrix z (Word -> Word -> Word)
--  , PrintDot (PhylogeneticDAG2 e (f String) u v w x y z)
  --, ToNewick (LeafSet (f String))
  --, ToNewick (PhylogeneticDAG2 e (f String) u v w x y z)
  ) => ToXML (PhylogeneticSolution (PhylogeneticDAG2 e (f String) u v w x y z)) where

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
                                   --, Right graphASCII
                                   ]
            -- TODO: This no longer works. Can't remember what I changed; pretty sure it's something simple.
            -- graphASCII           = xmlElement "Graphical" attrs graphASCIIContents
            -- graphASCIIContents   = (Right . toXML) <$> toList forests

            getDOT :: PrintDot a => PhylogeneticSolution a -> String
            getDOT = L.unpack . renderDot . toDot

            characterMetadata = xmlElement "Character_metadata" attrs metadataContents
            metadataContents  = [Right $ toXML metadataSequence]
                                -- [ Right . toXML $ fst metadataSequence
                                -- , Right . toXML $ snd metadataSequence
                                -- ]

            metadataSequence = hexmap f1 f2 f3 f4 f5 f6 arbitraryCharSeq
                where
                    arbitraryCharSeq = characterSequence . NE.head . resolutions . nodeDecoration $ arbitraryNode
                    arbitraryNode    = (references arbitraryRefDAG) ! arbitraryRootRef
                    arbitraryRootRef        = NE.head $ rootRefs arbitraryRefDAG
                    (PDAG2 arbitraryRefDAG) = NE.head arbitraryPDAG
                    arbitraryPDAG           = toNonEmpty $ NE.head forests
                    f1  = extractGeneralCharacterMetadata
                    f2  = extractDiscreteCharacterMetadata
                    f3  = extractDiscreteCharacterMetadata
                    f4  = g
                    f5  = g
                    f6  = g
                    g x = (generate dim scm, extractDiscreteCharacterMetadata x)
                        where
                            scm = uncurry $ x ^. symbolChangeMatrix
                            dim = length  $ x ^. characterAlphabet


