module File.Format.Conversion.ToInternal (convertGraph) where

import Prelude                      hiding ((++))
import File.Format.Newick.Internal  hiding (isLeaf)
import Bio.Phylogeny.Graph
import File.Format.Nexus.Parser
import Data.IntMap                  hiding (null, foldr, singleton)
import Data.Vector                  (fromList, (++), singleton, Vector)
import Data.Maybe                   (fromJust)
import Data.Monoid
import Data.List                    (elemIndex)
import Bio.Phylogeny.Tree.Node      hiding (isLeaf, isRoot)
import Bio.Phylogeny.Network        (isRoot, isLeaf)
import File.Format.Conversion.Encoder
import qualified Data.Map.Lazy as M

type Metadata = [String]

convertBothForest :: NewickForest -> [ParsedSequence] -> Graph
convertBothForest forest seqs = Graph $ zipWith convertBoth forest seqs

convertBoth :: NewickNode -> ParsedSequence -> Tree
convertBoth inTree seqs = 
    let metadata = makeEncodeInfo seqs
    in internalConvert inTree 0 mempty 0 seqs metadata

    where
        internalConvert :: NewickNode -> Int -> Tree -> Int -> ParsedSequence -> Vector CharInfo -> Tree
        internalConvert node pos initTree parentPos seqs metadata
            | null $ descendants node = newTree
            | otherwise = 
                let recurseTrees = fmap (\n -> internalConvert n (pos + (fromJust $ n `elemIndex` (descendants node))) initTree pos seqs metadata) (descendants node)
                in newTree <> foldr (\t acc -> acc <> t) mempty recurseTrees 
                where
                    name = case (newickLabel node) of
                                Nothing -> show pos
                                Just n -> n
                    newTaxa = insert pos name (taxaNodes initTree)
                    mySeq = case M.member name seqs of
                                True -> seqs M.! name
                                False -> mempty
                    myEncode = encodeIt mySeq metadata
                    myPack = packIt mySeq metadata
                    newNode = Node pos (isRoot node node) (isLeaf node node) ([pos..pos+(length $ descendants node)]) [parentPos] myEncode myPack mempty mempty mempty mempty 0
                    newTree = initTree {nodes = (nodes initTree) ++ (singleton newNode), taxaNodes = newTaxa}

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

convertSequences :: [ParsedSequence] -> Graph
convertSequences = Graph . fmap convertSeq

convertSeq :: ParsedSequence -> Tree
convertSeq seqs = undefined

convertAllInfo :: NewickForest -> [ParsedSequence] -> Metadata -> Graph
convertAllInfo forest seqs metadata = undefined