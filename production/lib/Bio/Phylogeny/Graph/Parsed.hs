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

module Bio.Phylogeny.Graph.Parsed where

import Prelude hiding ((++))
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Vector ((++))
import File.Format.Fasta
import File.Format.Fastc hiding (Identifier)
import           Data.Foldable
import File.Format.Newick
import File.Format.Nexus
import File.Format.TNT
import File.Format.TransitionCostMatrix
import qualified File.Format.VertexEdgeRoot.Parser as VER
import Bio.Phylogeny.Graph.Data
import Bio.Phylogeny.Graph.Utilities
import Bio.Phylogeny.Graph.Topological
import qualified Bio.Phylogeny.Tree.Node as N
import Bio.Phylogeny.Tree.Node.Topological (TopoNode(..))

import qualified Data.Set       as S  (Set, elemAt, toList, size, filter)
import qualified Data.HashMap.Lazy   as HM (HashMap, insert, (!))
import qualified Data.IntMap    as IM (IntMap, insert, (!))
import qualified Data.Vector    as V  (Vector, fromList, (!))
import qualified Data.IntSet    as IS (fromList)

class ParseGraph a where
    unifyGraph :: a -> Graph

instance ParseGraph NewickForest where
    unifyGraph = convertNewickToGraph

instance ParseGraph FastaParseResult where
    unifyGraph fasta = 
        let makeNames = foldr (\(FastaSequence label _) acc -> IM.insert (IM.size acc) label acc) mempty fasta
        in Graph $ pure $ mempty {nodeNames = makeNames}

instance ParseGraph FastcParseResult where
    unifyGraph fastc = 
        let makeNames = foldr (\(FastcSequence label _) acc -> IM.insert (IM.size acc) label acc) mempty fastc
        in Graph $ pure $ mempty {nodeNames = makeNames}

instance ParseGraph TntResult where
    unifyGraph (Left tree) = Graph $ toList $ fmap (fromTopo . flip TopoTree mempty . convertTNTToTopo True getTNTName) tree
    unifyGraph (Right (WithTaxa _ _ taxaTree)) = Graph $ toList $ fmap (fromTopo . flip TopoTree mempty . (convertTNTToTopo True fst)) taxaTree

instance ParseGraph TCM where
    unifyGraph = mempty

instance ParseGraph VER.VertexEdgeRoot where
    unifyGraph = convertVER

instance ParseGraph Nexus where
    unifyGraph = mempty

-- | Converts into a topo tree without sequences (main funcionality in newick conversion)
convertNewickToTopo :: NewickNode -> TopoTree
convertNewickToTopo tree0 = internalConvert tree0 True
    where
        internalConvert :: NewickNode -> Bool -> TopoTree
        --internalConvert inTree atRoot | trace ("internalConvert newick to topo " ++ show (descendants inTree)) False = undefined
        internalConvert inTree atRoot = 
            let 
                recurse = fmap (tree . flip internalConvert False) (descendants inTree) 
                myName = fromMaybe "HTU 0" (newickLabel inTree)
                myCost = fromMaybe 0 (branchLength inTree)
                node = TopoNode atRoot (null $ descendants inTree) myName mempty recurse mempty mempty mempty mempty mempty mempty myCost 0
            in --trace ("out from Newick to topo " ++ show node)
                TopoTree node mempty

-- | Converts a graph topology without sequences
convertNewickToGraph :: NewickForest -> Graph
convertNewickToGraph = Graph . fmap convertNewickToTree

-- | Converts a tree without sequences
convertNewickToTree :: NewickNode -> DAG
convertNewickToTree = fromTopo . convertNewickToTopo

-- | Converts a tnt tree into a topological form
convertTNTToTopo :: Bool -> (n -> String) -> LeafyTree n -> TopoNode b
convertTNTToTopo atRoot f (Leaf info) = TopoNode atRoot True (f info) mempty mempty mempty mempty mempty mempty mempty mempty 0 0
convertTNTToTopo atRoot f (Branch subtrees) = TopoNode atRoot False mempty mempty (map (convertTNTToTopo False f) subtrees) mempty mempty mempty mempty mempty mempty 0 0

-- | Conversion function for NodeType to string
getTNTName :: NodeType -> String
getTNTName node = case node of 
    (Index i) -> show i
    (Name n) -> n
    (Prefix s) -> s


data IntEdge = IntEdge (Int, Int) VER.EdgeLength deriving (Eq, Show, Ord)

convertVER :: VER.VertexEdgeRoot -> Graph
convertVER inVer = splitConnected outTree
    where
        nameDicts = accumNames 0 (VER.vertices inVer)
        rootList = S.toList $ VER.roots inVer
        finalNodes = accumNodes (VER.edges inVer) nameDicts
        outEdges = accumEdges finalNodes (VER.edges inVer) (fst nameDicts)
        outTree = DAG (fst nameDicts) mempty mempty finalNodes outEdges 0

        -- | First we correspond the names to indices in both directions
        accumNames :: Int -> S.Set VER.VertexLabel -> (IM.IntMap Identifier, HM.HashMap Identifier Int)
        --accumNames curPos labels | trace ("accumNames at pos " ++ show curPos ++ " with labels " ++ show labels) False = undefined
        accumNames curPos labels 
            | curPos == S.size labels = (mempty, mempty)
            | otherwise = 
                let 
                    recursion = accumNames (curPos + 1) labels
                    fromInt = IM.insert curPos (S.elemAt curPos labels) (fst recursion)
                    toInt = HM.insert (S.elemAt curPos labels) curPos (snd recursion)
                in (fromInt, toInt)

        -- | Now we accumulate as many nodes as we had names
        accumNodes :: S.Set VER.EdgeInfo -> (IM.IntMap Identifier, HM.HashMap Identifier Int) -> V.Vector NodeInfo
        --accumNodes inEdges (toNames, fromNames) | trace ("accumNodes with names " ++ show toNames) False = undefined
        accumNodes inEdges (toNames, fromNames) = fmap makeNode (V.fromList [0..S.size (VER.vertices inVer) - 1])
            where
                makeNode :: Int -> NodeInfo
                makeNode index = 
                    let
                        myName = toNames IM.! index
                        myParents = foldr (\(VER.EdgeInfo (o, t) _) acc -> if t == myName then (fromNames HM.! o) : acc else acc) mempty inEdges
                        myChildren = foldr (\(VER.EdgeInfo (o, t) _) acc -> if o == myName then (fromNames HM.! t) : acc else acc) mempty inEdges
                        atRoot = myName `elem` rootList
                    in N.Node index atRoot (null myChildren) myParents myChildren mempty mempty mempty mempty mempty mempty 0 0

        -- | Now we can generate the edges
        accumEdges :: V.Vector NodeInfo -> S.Set VER.EdgeInfo -> IM.IntMap Identifier -> V.Vector EdgeSet
        accumEdges madeNodes origEdges nameMap = fmap makeEdge madeNodes
            where
                getLen :: Int -> Int -> Double
                getLen start stop = 
                    let findIt = S.toList $ S.filter (\(VER.EdgeInfo (i, o) _) -> i == (nameMap IM.! start) && o == (nameMap IM.! stop)) origEdges
                    in case findIt of
                        [VER.EdgeInfo _ (Just l)] -> l
                        _ -> 0

                makeEdge :: NodeInfo -> EdgeSet
                makeEdge inNode = 
                    let
                        myI = N.code inNode
                        inE = IS.fromList $ N.parents inNode
                        outInfo = map (\i -> EdgeInfo (getLen myI i) (madeNodes V.! myI) (madeNodes V.! i) Nothing) (N.children inNode)
                        outMap = foldr (\info acc -> IM.insert (N.code $ terminal info) info acc) mempty outInfo
                    in EdgeSet inE outMap


