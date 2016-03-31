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


import qualified Bio.Phylogeny.Graph.Data as G
import           Bio.Phylogeny.Graph.Topological
import           Bio.Phylogeny.Graph.Utilities
import           Bio.Phylogeny.Solution
import qualified Bio.Phylogeny.Tree.Node as N
import           Bio.Phylogeny.Tree.Node.Topological (TopoNode(..))
import           Data.Foldable
import qualified Data.HashMap.Lazy  as HM (HashMap, insert, (!))
import qualified Data.Set           as S  (Set, elemAt, toList, size, filter)
import qualified Data.IntMap        as IM (IntMap, insert, size, (!))
import qualified Data.IntSet        as IS (fromList)
import           Data.Map                 (keys)
import           Data.Maybe
import qualified Data.Vector        as V  (Vector, fromList, (!))
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
--    unifyGraph (Left                   forest) = toNewDag . G.Graph . toList $ fmap (fromTopo . flip TopoTree mempty . convertTNTToTopo True getTNTName) forest
--    unifyGraph (Right (WithTaxa _ _ taxaTree)) = toNewDag . G.Graph . toList $ fmap (fromTopo . flip TopoTree mempty . (convertTNTToTopo True fst)) taxaTree

instance ParseGraph TCM where
    unifyGraph = mempty

instance ParseGraph VER.VertexEdgeRoot where
    unifyGraph = convertVerToNewick

instance ParseGraph Nexus where
    unifyGraph = mempty -- Will also be newick forest at somepoint

convertVerToNewick :: VertexEdgeRoot -> Forest NewickNode
convertVerToNewick (VER v e r) = buildNewickTree Nothing <$> toList r
  where
    buildNewickTree :: Maybe Double -> VertexLabel -> NewickNode
    buildNewickTree c n = fromJust $ newickNode kids (Just n) c
      where
        kids = fmap f . filter ((==n) . VER.edgeOrigin) $ toList e
        f    = buildNewickTree <$> VER.edgeLength <*> VER.edgeTarget

convertTntToNewick :: (n -> String) -> LeafyTree n -> NewickNode
convertTntToNewick f (Leaf   x ) = fromJust $ newickNode [] (Just $ f x) Nothing -- Scary use of fromJust?
convertTntToNewick f (Branch xs) = fromJust $ newickNode (convertTntToNewick f <$> xs) Nothing Nothing

-- | Conversion function for NodeType to string
getTNTName :: NodeType -> String
getTNTName node = case node of 
    (Index i) -> show i
    (Name n) -> n
    (Prefix s) -> s

{-
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
                node = TopoNode atRoot (null $ descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty myCost 0
            in --trace ("out from Newick to topo " ++ show node)
                TopoTree node mempty

-- | Converts a graph topology without sequences
convertNewickToGraph :: NewickForest -> G.Graph
convertNewickToGraph = G.Graph . fmap convertNewickToTree

-- | Converts a tree without sequences
convertNewickToTree :: NewickNode -> G.DAG
convertNewickToTree = fromTopo . convertNewickToTopo

-- | Converts a tnt tree into a topological form
convertTNTToTopo :: Bool -> (n -> String) -> LeafyTree n -> TopoNode b
convertTNTToTopo atRoot f (Leaf   info    ) = TopoNode atRoot True (f info) mempty mempty mempty mempty mempty mempty mempty 0 0
convertTNTToTopo atRoot f (Branch subtrees) = TopoNode atRoot False mempty (map (convertTNTToTopo False f) subtrees) mempty mempty mempty mempty mempty mempty 0 0

data IntEdge = IntEdge (Int, Int) VER.EdgeLength deriving (Eq, Show, Ord)

convertVER :: VER.VertexEdgeRoot -> G.Graph
convertVER inVer = splitConnected outTree
    where
        nameDicts = accumNames 0 (VER.vertices inVer)
        rootList = S.toList $ VER.roots inVer
        finalNodes = accumNodes (VER.edges inVer) nameDicts
        outEdges = accumEdges finalNodes (VER.edges inVer) (fst nameDicts)
        outTree = G.DAG (fst nameDicts) mempty mempty finalNodes outEdges 0

        -- | First we correspond the names to indices in both directions
        accumNames :: Int -> S.Set VER.VertexLabel -> (IM.IntMap G.Identifier, HM.HashMap G.Identifier Int)
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
        accumNodes :: S.Set VER.EdgeInfo -> (IM.IntMap G.Identifier, HM.HashMap G.Identifier Int) -> V.Vector G.NodeInfo
        --accumNodes inEdges (toNames, fromNames) | trace ("accumNodes with names " ++ show toNames) False = undefined
        accumNodes inEdges (toNames, fromNames) = fmap makeNode (V.fromList [0..S.size (VER.vertices inVer) - 1])
            where
                makeNode :: Int -> G.NodeInfo
                makeNode index = 
                    let
                        myName = toNames IM.! index
                        myParents = foldr (\(VER.EdgeInfo (o, t) _) acc -> if t == myName then (fromNames HM.! o) : acc else acc) mempty inEdges
                        myChildren = foldr (\(VER.EdgeInfo (o, t) _) acc -> if o == myName then (fromNames HM.! t) : acc else acc) mempty inEdges
                        atRoot = myName `elem` rootList
                    in N.Node index (toNames IM.! index) atRoot (null myChildren) myParents myChildren mempty mempty mempty mempty mempty mempty 0 0

        -- | Now we can generate the edges
        accumEdges :: V.Vector G.NodeInfo -> S.Set VER.EdgeInfo -> IM.IntMap G.Identifier -> V.Vector G.EdgeSet
        accumEdges madeNodes origEdges nameMap = fmap makeEdge madeNodes
            where
                getLen :: Int -> Int -> Double
                getLen start stop = 
                    let findIt = S.toList $ S.filter (\(VER.EdgeInfo (i, o) _) -> i == (nameMap IM.! start) && o == (nameMap IM.! stop)) origEdges
                    in case findIt of
                        [VER.EdgeInfo _ (Just l)] -> l
                        _ -> 0

                makeEdge :: G.NodeInfo -> G.EdgeSet
                makeEdge inNode = 
                    let
                        myI = N.code inNode
                        inE = IS.fromList $ N.parents inNode
                        outInfo = map (\i -> G.EdgeInfo (getLen myI i) (madeNodes V.! myI) (madeNodes V.! i) Nothing) (N.children inNode)
                        outMap = foldr (\info acc -> IM.insert (N.code $ G.terminal info) info acc) mempty outInfo
                    in G.EdgeSet inE outMap
-}

toNewDag :: G.Graph -> Forest DAG
toNewDag (G.Graph xs) = f <$> xs
  where
    f :: G.DAG -> DAG
    f = DAG <$> G.nodes <*> (\x -> e <$> G.edges x) <*> G.root
    e :: G.EdgeSet -> EdgeSet
    e = EdgeSet <$> G.inNodes <*> (\x -> i <$> G.outNodes x)
    i :: G.EdgeInfo -> EdgeInfo
    i = EdgeInfo <$> G.len <*> G.origin <*> G.terminal <*> G.virtualNode
