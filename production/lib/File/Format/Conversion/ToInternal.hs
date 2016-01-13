{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module File.Format.Conversion.ToInternal (convertGraph) where

import Prelude                      hiding ((++))
import File.Format.Newick.Internal  hiding (isLeaf)
import Bio.Phylogeny.Graph
import Data.BitVector               (BitVector)
import Data.IntMap                  hiding (null, foldr, singleton)
import Data.Vector                  ((++), singleton)
import qualified Data.Vector as V   (foldr)
import Data.Maybe                   (fromJust)
import Data.Monoid
import Data.List                    (elemIndex)
import Bio.Phylogeny.Tree.Node      hiding (isLeaf, isRoot)
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
        internalConvert :: NewickNode -> Int -> Tree -> Int -> TreeSeqs -> SimpleMetadata -> Tree
        internalConvert node pos initTree parentPos inSeqs metadata
            | null $ descendants node = newTree
            | otherwise = 
                let recurseTrees = fmap (\n -> internalConvert n (pos + (fromJust $ n `elemIndex` (descendants node))) initTree pos seqs metadata) (descendants node)
                in newTree <> foldr (\t acc -> acc <> t) mempty recurseTrees 
                where
                    name = case (newickLabel node) of
                                Nothing -> show pos
                                Just n -> n
                    newTaxa = insert pos name (taxaNodes initTree)
                    mySeq = case M.member name inSeqs of
                                True -> inSeqs M.! name
                                False -> mempty
                    myEncode = encodeIt mySeq metadata
                    myPack = packIt mySeq metadata
                    newNode = Node pos (isRoot node node) (isLeaf node node) ([pos..pos+(length $ descendants node)]) [parentPos] (singleton myEncode) (singleton myPack) mempty mempty mempty mempty 0
                    newTree = initTree {nodes = (nodes initTree) ++ (singleton newNode), taxaNodes = newTaxa}

        makeEncodeInfo :: TreeSeqs -> SimpleMetadata
        makeEncodeInfo inSeqs = M.foldr (\nseq acc -> getNodeAlph nseq acc) mempty inSeqs
            where
                getNodeAlph :: ParsedSeq -> SimpleMetadata -> SimpleMetadata
                getNodeAlph inSeq soFar = V.foldr (\cIn cPrev -> foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev) cPrev cIn) soFar inSeq


        encodeIt :: ParsedSeq -> SimpleMetadata -> EncodedSeq BitVector
        encodeIt = encodeOverAlphabet

        packIt :: ParsedSeq -> SimpleMetadata -> EncodedSeq BitVector
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
                    newTaxa = insert pos name (taxaNodes initTree)
                    newNode = Node pos (isRoot node node) (isLeaf node node) ([pos..pos+(length $ descendants node)]) [parentPos] mempty mempty mempty mempty mempty mempty 0
                    newTree = initTree {nodes = (nodes initTree) ++ (singleton newNode), taxaNodes = newTaxa}

--convertSequences :: [ParsedSeq] -> Graph
--convertSequences = Graph . fmap convertSeq

--convertSeq :: ParsedSeq -> Tree
--convertSeq seqs = undefined

--convertAllInfo :: NewickForest -> [ParsedSeq] -> Metadata -> Graph
--convertAllInfo forest seqs metadata = undefined