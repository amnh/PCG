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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module Bio.Graph
  ( PhylogeneticSolution(..)
  , PhylogeneticForest(..)
  , phylogeneticForests
  ) where

-- import Bio.Graph.Component
import Bio.Graph.Forest
import Bio.Graph.LeafSet
-- import Bio.Graph.Network
-- import Bio.Graph.Tree
import           Control.Lens        hiding (Indexable)
import           Data.Foldable
import           Data.Key
import           Data.List
import           Data.List.NonEmpty         (NonEmpty)
-- import qualified Data.List.NonEmpty  as NE
import           Data.Semigroup
import           Prelude             hiding (lookup)
import           Text.XML.Custom
import           Text.XML.Light


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


instance Show a => Show (PhylogeneticSolution a) where

    show = ("Solution:\n\n" <>) . indent . renderForests . fmap renderForest . phylogeneticForests
      where
        indent = intercalate "\n" . fmap ("  " <>) . lines
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


-- instance (Foldable s, HasLeafSet (s n) (LeafSet n), ToXML n) => ToXML (PhylogeneticSolution (s n)) where

--     toXML (PhylogeneticSolution soln) = xmlElement "Solution" attrs contents
--         where
--             attrs    = []
--             contents = [ Right $ collapseElemList "Leaf sets"   attrs leafSets
--                        -- , Right graphRepresentation
--                        , Right $ collapseElemList "Final graph" attrs soln
--                        ]
--             -- (PhylogeneticForest firstForest) = head $ toList soln
--             -- (PDAG2 refDag _e _n)    = head $ toList firstForest
--             -- (refDag )
--             -- leaves   = collapseElemList "Leaf sets" attrs leafSets
--             leafSets = (^. leafSets) <$> soln


instance HasLeafSet a (LeafSet b) => HasLeafSet (PhylogeneticSolution a) (NonEmpty (PhylogeneticForest b)) where

    leafSet = lens getter setter
        where
            getter e   = (^. leafSet) <$> (stuff e)
            setter e _ = id e
            stuff (PhylogeneticSolution thing)     = thing