-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Sanity
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module containing sanity checks for a graph that the different components agree and do as expected
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph.Sanity where

import           Bio.Phylogeny.Graph.Data
import           Bio.Phylogeny.Tree.Node
import           Data.Foldable
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import           Data.Vector ((!))

-- | Wrapper function to do all sanity checks
sanity :: Graph -> Bool
sanity (Graph inDags) = foldr (\d acc -> acc && operateAll d) True inDags
    where
        operateAll d = crossCheckEdges d && verifyRoot d && verifyCharLens d

-- | Cross-check that the edges correspond
crossCheckEdges :: DAG -> Bool
crossCheckEdges = V.and . zipDAG
    where
        zipDAG d = V.zipWith (\n e -> IS.toList (inNodes  e) == parents n) (nodes d) (edges d)

-- | Check that the root is a real node and has no parents
verifyRoot :: DAG -> Bool
verifyRoot d = length (nodes d) >= root d && (null . parents $ nodes d ! root d)

-- | Check that there's the same amount of characters stored in parsed, encoded, and metadata
verifyCharLens :: DAG -> Bool
verifyCharLens curDAG = 
    let
        metadataLen = V.length $ characters curDAG
        encodedLens = fmap (length . encoded) $ nodes curDAG
        parsedLens  = fmap length $ uparsedSeqs curDAG
        parsedAgainstMeta  = foldr (\v acc -> acc && v == metadataLen) True parsedLens
        encodedAgainstMeta = foldr (\v acc -> acc && v == metadataLen) True encodedLens
    in parsedAgainstMeta && encodedAgainstMeta

