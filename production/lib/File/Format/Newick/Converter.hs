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
convertBoth inTree seqs = 
    let metadata = makeEncodeInfo seqs
    in internalConvert inTree 0 mempty 0 seqs metadata

    where
        internalConvert :: NewickNode -> Int -> Tree -> Int -> TreeSeqs -> Vector SimpleMetadata -> Tree
        internalConvert node pos initTree parentPos inSeqs metadata
            | null $ descendants node = newTree
            | otherwise = 
                let recurseTrees = fmap (\n -> internalConvert n (pos + (fromJust $ n `elemIndex` (descendants node))) initTree pos seqs metadata) (descendants node)
                in newTree <> foldr (flip (<>)) mempty recurseTrees 
                where
                    name     = fromMaybe (show pos) (newickLabel node)
                    newTaxa  = insert pos name (nodeNames initTree)
                    mySeq    = if M.member name inSeqs
                               then inSeqs M.! name
                               else mempty
                    myEncode = encodeIt mySeq metadata
                    myPack   = packIt mySeq metadata
                    newNode  = Node pos (isRoot node node) (isLeaf node node) [pos..pos + length (descendants node)] [parentPos] (singleton myEncode) (singleton myPack) mempty mempty mempty mempty 0
                    newTree  = initTree {nodes = nodes initTree ++ singleton newNode, nodeNames = newTaxa}

        makeEncodeInfo :: TreeSeqs -> Vector SimpleMetadata
        makeEncodeInfo = M.foldr getNodeAlph mempty
            where
                getNodeAlph :: ParsedSequences -> Vector SimpleMetadata -> Vector SimpleMetadata
                getNodeAlph = V.zipWith getNodeAlphAt

                getNodeAlphAt :: ParsedSeq -> SimpleMetadata -> SimpleMetadata
                getNodeAlphAt inSeq soFar = V.foldr (\cIn cPrev -> foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev) cPrev cIn) soFar inSeq


        encodeIt :: ParsedSequences -> Vector SimpleMetadata -> EncodedSeq BitVector
        encodeIt = encodeOverAlphabet

        packIt :: ParsedSequences -> Vector SimpleMetadata -> EncodedSeq BitVector
        packIt = encodeOverAlphabet

convertGraph :: NewickForest -> Graph
convertGraph = Graph . fmap convertTree

convertTree :: NewickNode -> Tree
convertTree inTree = internalConvert inTree 0 mempty 0
    where
        internalConvert :: NewickNode -> Int -> Tree -> Int -> Tree
        internalConvert node pos initTree parentPos 
            | null $ descendants node = newTree
            | otherwise = 
                let recurseTrees = fmap (\n -> internalConvert n (pos + (fromJust $ n `elemIndex` (descendants node))) initTree pos) (descendants node)
                in newTree <> foldr (\t acc -> acc <> t) mempty recurseTrees 
                where
                    name = case (newickLabel node) of
                                Nothing -> show pos
                                Just n -> n
                    newTaxa = insert pos name (nodeNames initTree)
                    newNode = Node pos (isRoot node node) (isLeaf node node) ([pos..pos+(length $ descendants node)]) [parentPos] mempty mempty mempty mempty mempty mempty 0
                    newTree = initTree {nodes = (nodes initTree) ++ (singleton newNode), nodeNames = newTaxa}

convertTopo :: NewickForest -> TopoGraph 
convertTopo = TopoGraph . fmap convertTopoTree

convertTopoTree :: NewickNode -> TopoTree
convertTopoTree inTree = internalConvert inTree True
    where
        internalConvert :: NewickNode -> Bool -> TopoTree
        internalConvert inTree atRoot = 
            let 
                recurse = fmap (tree . flip internalConvert False) (descendants inTree) 
                myName = case newickLabel inTree of
                            Just label -> label
                            Nothing -> ""
                myCost = case branchLength inTree of
                            Just len -> len
                            Nothing -> 0
                node = TN.TopoNode atRoot (null $ descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty myCost
            in TopoTree node mempty
