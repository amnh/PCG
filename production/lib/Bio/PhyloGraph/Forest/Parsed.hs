-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Forest.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for a parsed forest so that it can convert into an internal forest.
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.PhyloGraph.Forest.Parsed where

import           Bio.PhyloGraph.Forest.Internal
import           Data.Foldable
import           Data.Maybe
import           File.Format.Fasta
import           File.Format.Fastc                 hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                 hiding (TaxonSequenceMap)
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot.Parser        (VertexEdgeRoot(..),VertexLabel)
import qualified File.Format.VertexEdgeRoot.Parser as VER

-- | Represents a parser result type which can have a possibly empty forest
--   extracted from it.
class ParsedForest a where
    unifyGraph :: a -> Forest NewickNode

instance ParsedForest NewickForest where
    unifyGraph = id --toNewDag . convertNewickToGraph

instance ParsedForest FastaParseResult where
    unifyGraph = const mempty

instance ParsedForest TaxonSequenceMap where
    unifyGraph = const mempty

instance ParsedForest FastcParseResult where
    unifyGraph = const mempty

instance ParsedForest TntResult where
    unifyGraph (Left                forest ) = (convertTntToNewick getTNTName) <$> toList forest
    unifyGraph (Right (WithTaxa _ _ forest)) = (convertTntToNewick fst       ) <$> toList forest

instance ParsedForest TCM where
    unifyGraph = mempty

instance ParsedForest VER.VertexEdgeRoot where
    unifyGraph = convertVerToNewick

instance ParsedForest Nexus where
    unifyGraph = mempty -- Will also be newick forest at somepoint

-- | Convert the referential forests defined by sets of verticies, edges, and
--   roots into a forest of topological tree structure.
convertVerToNewick :: VertexEdgeRoot -> Forest NewickNode
convertVerToNewick (VER _ e r) = buildNewickTree Nothing <$> toList r
  where
    buildNewickTree :: Maybe Double -> VertexLabel -> NewickNode
    buildNewickTree c n = fromJust $ newickNode kids (Just n) c
      where
        kids = fmap (uncurry buildNewickTree) . catMaybes $ kidMay n <$> toList e
        kidMay vertex edge = do
          other <- VER.connectedVertex vertex edge
          pure (VER.edgeLength edge, other)

-- | Convert the topological 'LeafyTree' structure into a forest of topological
--   tree structures with nodes that hold additional information.
convertTntToNewick :: (n -> String) -> LeafyTree n -> NewickNode
convertTntToNewick f (Leaf   x ) = fromJust $ newickNode [] (Just $ f x) Nothing -- Scary use of fromJust?
convertTntToNewick f (Branch xs) = fromJust $ newickNode (convertTntToNewick f <$> xs) Nothing Nothing

-- | Conversion function for NodeType to string
getTNTName :: NodeType -> String
getTNTName node = case node of 
    (Index i) -> show i
    (Name n) -> n
    (Prefix s) -> s

