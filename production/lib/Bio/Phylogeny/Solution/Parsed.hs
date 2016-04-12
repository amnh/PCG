-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for a parsed graph so that it can convert into an internal graph
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Phylogeny.Solution.Parsed where


import           Bio.Phylogeny.Solution
import           Data.Foldable
import           Data.Maybe
import           File.Format.Fasta
import           File.Format.Fastc hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot.Parser    (VertexEdgeRoot(..),VertexLabel)
import qualified File.Format.VertexEdgeRoot.Parser as VER
import           Prelude           hiding ((++))

class ParseGraph a where
    unifyGraph :: a -> Forest NewickNode

instance ParseGraph NewickForest where
    unifyGraph = id --toNewDag . convertNewickToGraph

instance ParseGraph FastaParseResult where
    unifyGraph = const mempty
--    unifyGraph fasta = 
--        let makeNames = foldr (\(FastaSequence label _) acc -> IM.insert (IM.size acc) label acc) mempty fasta
--        in G.Graph $ pure $ mempty { G.nodeNames = makeNames}

instance ParseGraph TaxonSequenceMap where
    unifyGraph = const mempty
--    unifyGraph fasta = 
--        let makeNames = foldr (\label acc -> IM.insert (IM.size acc) label acc) mempty $ keys fasta
--        in G.Graph $ pure $ mempty { G.nodeNames = makeNames}

instance ParseGraph FastcParseResult where
    unifyGraph = const mempty
--    unifyGraph fastc = 
--        let makeNames = foldr (\(FastcSequence label _) acc -> IM.insert (IM.size acc) label acc) mempty fastc
--        in G.Graph $ pure $ mempty { G.nodeNames = makeNames}

instance ParseGraph TntResult where
    unifyGraph (Left                forest ) = (convertTntToNewick getTNTName) <$> toList forest
    unifyGraph (Right (WithTaxa _ _ forest)) = (convertTntToNewick fst       ) <$> toList forest

instance ParseGraph TCM where
    unifyGraph = mempty

instance ParseGraph VER.VertexEdgeRoot where
    unifyGraph = convertVerToNewick

instance ParseGraph Nexus where
    unifyGraph = mempty -- Will also be newick forest at somepoint

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

convertTntToNewick :: (n -> String) -> LeafyTree n -> NewickNode
convertTntToNewick f (Leaf   x ) = fromJust $ newickNode [] (Just $ f x) Nothing -- Scary use of fromJust?
convertTntToNewick f (Branch xs) = fromJust $ newickNode (convertTntToNewick f <$> xs) Nothing Nothing

-- | Conversion function for NodeType to string
getTNTName :: NodeType -> String
getTNTName node = case node of 
    (Index i) -> show i
    (Name n) -> n
    (Prefix s) -> s

