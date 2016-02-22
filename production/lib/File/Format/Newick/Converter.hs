-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nweick.Converter
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

module File.Format.Newick.Converter (convertGraph) where

import Prelude                      hiding ((++))
import File.Format.Newick.Internal  hiding (isLeaf)
import Bio.Phylogeny.Graph
import Control.Monad
import Data.BitVector               (BitVector)
import Data.Vector                  (Vector)
import qualified Data.Vector as V   (foldr, zipWith)
import Data.Maybe
import Data.Monoid                  ()

import Bio.Phylogeny.Graph.Topological
import qualified Bio.Phylogeny.Tree.Node.Topological as TN
import Bio.Phylogeny.Network        ()
import Bio.Sequence.Parsed
import Bio.Sequence.Coded
-- import File.Format.Conversion.Encoder
import qualified Data.Map.Lazy as M 

type SimpleMetadata = [String]

-- | Convert a newick graph with associated sequences
convertBothForest :: NewickForest -> [TreeSeqs] -> Graph
convertBothForest forest seqs = Graph $ zipWith convertBoth forest seqs

-- | Convert a newick tree with associated sequences
convertBoth :: NewickNode -> TreeSeqs -> Tree
convertBoth inTree seqs = fromTopo $ convertBothTopo inTree seqs

-- | If encoding info is not already present, functionality to scrape it from sequences
makeEncodeInfo :: TreeSeqs -> Vector SimpleMetadata
makeEncodeInfo = M.foldr getNodeAlph mempty
    where
        getNodeAlph :: ParsedSequences -> Vector SimpleMetadata -> Vector SimpleMetadata
        getNodeAlph = V.zipWith getNodeAlphAt

        getNodeAlphAt :: Maybe ParsedSeq -> SimpleMetadata -> SimpleMetadata
        getNodeAlphAt inSeq soFar
            | isNothing inSeq = mempty
            | otherwise =  V.foldr (\cIn cPrev -> foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev) cPrev cIn) soFar (fromJust inSeq)

-- | Wrapper for encoding to get types right
encodeIt :: ParsedSequences -> Alphabet -> EncodedSequences BitVector
encodeIt inSeqs alphs = V.zipWith (\s a -> join $ (flip encodeOverAlphabet) a <$> s) inSeqs alphs 

-- | Wrapper for packing to get types right
packIt :: ParsedSequences -> Alphabet -> EncodedSequences BitVector
packIt inSeqs alphs = V.zipWith (\s a -> join $ (flip encodeOverAlphabet) a <$> s) inSeqs alphs

-- | Convert a newick tree and associated sequences to a topo tree (where funcionality is located)
convertBothTopo :: NewickNode -> TreeSeqs -> TopoTree
convertBothTopo rootTree inSeqs = internalConvert True rootTree
    where 
        metadata = makeEncodeInfo inSeqs

        internalConvert :: Bool -> NewickNode -> TopoTree
        internalConvert atRoot inTree = 
            let 
                recurse = fmap (tree . internalConvert False) (descendants inTree) 
                myName = case newickLabel inTree of
                            Just label -> label
                            Nothing -> ""
                myCost = case branchLength inTree of
                            Just l -> l
                            Nothing -> 0
                mySeq = if M.member myName inSeqs
                            then inSeqs M.! myName
                            else mempty
                myEncode = encodeIt mySeq metadata
                myPack   = packIt mySeq metadata
                node = TN.TopoNode atRoot (null $ descendants inTree) myName mySeq recurse myEncode myPack mempty mempty mempty mempty myCost
            in TopoTree node mempty

-- | Converts a graph topology without sequences
convertGraph :: NewickForest -> Graph
convertGraph = Graph . fmap convertTree

-- | Converts a tree without sequences
convertTree :: NewickNode -> Tree
convertTree = fromTopo . convertTopoTree

-- | Converts into a topo graph without sequences
convertTopo :: NewickForest -> TopoGraph 
convertTopo = TopoGraph . fmap convertTopoTree

-- | Converts into a topo tree without sequences (main funcionality in newick conversion)
convertTopoTree :: NewickNode -> TopoTree
convertTopoTree tree0 = internalConvert tree0 True
    where
        internalConvert :: NewickNode -> Bool -> TopoTree
        internalConvert inTree atRoot = 
            let 
                recurse = fmap (tree . flip internalConvert False) (descendants inTree) 
                myName = case newickLabel inTree of
                            Just label -> label
                            Nothing -> ""
                myCost = case branchLength inTree of
                            Just l -> l
                            Nothing -> 0
                node = TN.TopoNode atRoot (null $ descendants inTree) myName mempty recurse mempty mempty mempty mempty mempty mempty myCost
            in TopoTree node mempty
