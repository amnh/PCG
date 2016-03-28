-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Newick.Converter
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality for converting a newick parsed format into an internal graph format
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module File.Format.Newick.Converter where

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.Graph.Topological
import qualified Bio.Phylogeny.Tree.Node.Topological as TN
import           Bio.Phylogeny.Network                     ()
import           Bio.Sequence.Parsed
import qualified Data.Map.Lazy                      as M 
import           Data.Maybe
import           Data.Monoid                               ()
import           File.Format.Conversion.Encoder
import           File.Format.Newick.Internal        hiding (isLeaf)

--import Debug.Trace

type SimpleMetadata = [String]

-- TODO: add functionality to detect if some terminal has no sequence

-- | Convert a newick graph with associated sequences
convertBothForest :: NewickForest -> [TreeSeqs] -> Graph
convertBothForest forest seqs = Graph $ zipWith convertBoth forest seqs

-- | Convert a newick tree with associated sequences
convertBoth :: NewickNode -> TreeSeqs -> DAG
convertBoth inTree seqs = fromTopo $ convertBothTopo inTree seqs

-- | Convert a newick tree and associated sequences to a topo tree (where funcionality is located)
convertBothTopo :: NewickNode -> TreeSeqs -> TopoTree
convertBothTopo rootTree inSeqs = internalConvert True rootTree
    where 
        fullMetadata = makeEncodeInfo inSeqs

        internalConvert :: Bool -> NewickNode -> TopoTree
        internalConvert atRoot inTree = 
            let 
                recurse = fmap (tree . internalConvert False) (descendants inTree) 
                myName = fromMaybe "HTU 0" $ newickLabel  inTree
                myCost = fromMaybe 0       $ branchLength inTree
                mySeq = if M.member myName inSeqs
                            then inSeqs M.! myName
                            else mempty
                myEncode = encodeIt mySeq fullMetadata
                myPack   = packIt mySeq fullMetadata
                node = TN.TopoNode atRoot (null $ descendants inTree) myName mySeq recurse myEncode myPack mempty mempty mempty mempty myCost 0
            in --trace ("internalConvert with metadata " ++ show fullMetadata)
                TopoTree node fullMetadata

-- | Converts a graph topology without sequences
convertGraph :: NewickForest -> Graph
convertGraph = Graph . fmap convertTree

-- | Converts a tree without sequences
convertTree :: NewickNode -> DAG
convertTree = fromTopo . convertTopoTree

-- | Converts into a topo graph without sequences
convertTopo :: NewickForest -> TopoGraph 
convertTopo = TopoGraph . fmap convertTopoTree

-- | Converts into a topo tree without sequences (main funcionality in newick conversion)
convertTopoTree :: NewickNode -> TopoTree
--convertTopoTree tree0 | trace ("NewickNode to topo " ++ show tree0) False = undefined
convertTopoTree tree0 = internalConvert tree0 True
    where
        internalConvert :: NewickNode -> Bool -> TopoTree
        --internalConvert inTree atRoot | trace ("internalConvert newick to topo " ++ show (descendants inTree)) False = undefined
        internalConvert inTree atRoot = 
            let 
                recurse = fmap (tree . flip internalConvert False) (descendants inTree) 
                myName = fromMaybe "HTU 0" (newickLabel inTree)
                myCost = fromMaybe 0 (branchLength inTree)
                node = TN.TopoNode atRoot (null $ descendants inTree) myName mempty recurse mempty mempty mempty mempty mempty mempty myCost 0
            in --trace ("out from Newick to topo " ++ show node)
                TopoTree node mempty
