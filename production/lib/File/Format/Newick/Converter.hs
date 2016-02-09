{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module File.Format.Newick.Converter (convertGraph) where

import Prelude                      hiding ((++))
import File.Format.Newick.Internal  hiding (isLeaf)
import Bio.Phylogeny.Graph
import Data.BitVector               (BitVector)
import Data.IntMap                  hiding (null, foldr, singleton)
import Data.Vector                  (Vector,(++), singleton)
import qualified Data.Vector as V   (foldr, zipWith)
import Data.Maybe                   (fromJust,fromMaybe)
import Data.Monoid
import Data.List                    (elemIndex)

import Bio.Phylogeny.Graph.Topological
import Bio.Phylogeny.Tree.Node      hiding (isLeaf, isRoot)
import qualified Bio.Phylogeny.Tree.Node.Topological as TN
import Bio.Phylogeny.Network        (isRoot, isLeaf)
import Bio.Sequence.Parsed
import Bio.Sequence.Coded
-- import File.Format.Conversion.Encoder
import qualified Data.Map.Lazy as M 

type SimpleMetadata = [String]

convertBothForest :: NewickForest -> [TreeSeqs] -> Graph
convertBothForest forest seqs = Graph $ zipWith convertBoth forest seqs

convertBoth :: NewickNode -> TreeSeqs -> Tree
convertBoth inTree seqs = fromTopo $ convertBothTopo inTree seqs

makeEncodeInfo :: TreeSeqs -> Vector SimpleMetadata
makeEncodeInfo = M.foldr getNodeAlph mempty
    where
        getNodeAlph :: ParsedSequences -> Vector SimpleMetadata -> Vector SimpleMetadata
        getNodeAlph = V.zipWith getNodeAlphAt

        getNodeAlphAt :: ParsedSeq -> SimpleMetadata -> SimpleMetadata
        getNodeAlphAt inSeq soFar = V.foldr (\cIn cPrev -> foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev) cPrev cIn) soFar inSeq


encodeIt :: ParsedSequences -> Alphabet -> EncodedSequences BitVector
encodeIt inSeqs alphs = V.zipWith encodeOverAlphabet inSeqs alphs 

packIt :: ParsedSequences -> Alphabet -> EncodedSequences BitVector
packIt inSeqs alphs = V.zipWith encodeOverAlphabet inSeqs alphs 

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
                node = TN.TopoNode atRoot (null $ descendants inTree) myName recurse myEncode myPack mempty mempty mempty mempty myCost
            in TopoTree node mempty


convertGraph :: NewickForest -> Graph
convertGraph = Graph . fmap convertTree

convertTree :: NewickNode -> Tree
convertTree = fromTopo . convertTopoTree

convertTopo :: NewickForest -> TopoGraph 
convertTopo = TopoGraph . fmap convertTopoTree

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
                node = TN.TopoNode atRoot (null $ descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty myCost
            in TopoTree node mempty
