-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a DAG
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.DAG (module Bio.PhyloGraph.DAG.Internal, module Bio.PhyloGraph.DAG.Class, fromNewick, fromTopo, toTopo) where

import           Bio.PhyloGraph.DAG.Internal
import           Bio.PhyloGraph.DAG.Class
import           Bio.PhyloGraph.Edge
import           Bio.PhyloGraph.Forest
import qualified Bio.PhyloGraph.Network as N
import qualified Bio.PhyloGraph.Network.Subsettable as SN
import           Bio.PhyloGraph.Node
import qualified Bio.PhyloGraph.Node.Topological as TN
import qualified Bio.PhyloGraph.Tree.EdgeAware as ET
import           Bio.PhyloGraph.Tree.Binary
import qualified Bio.PhyloGraph.Tree.Referential as RT
import           Bio.PhyloGraph.Tree.Rose
import           Data.Bifunctor
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import           Data.Key (lookup)
import           Data.Maybe
import           Data.Monoid
import           Data.Vector ((//), Vector, elemIndex, (!))
import qualified Data.Vector as V
import qualified File.Format.Newick as New
import           Prelude hiding (lookup)
import           Safe
import           Test.Tasty.QuickCheck

instance StandardDAG DAG NodeInfo EdgeSet where
    getNodes       = nodes
    setNodes inD n = inD {nodes = n}
    getEdges       = edges
    setEdges inD e = inD {edges = e}
    getRoot  inD   = (nodes inD) ! (root inD)

instance Arbitrary DAG where
  arbitrary = fromTopo <$> (arbitrary :: Gen TopoDAG)

instance Arbitrary TopoDAG where
  arbitrary = undefined

instance Monoid DAG where
    mempty = DAG mempty mempty 0
    mappend dag1 dag2 = appendAt dag1 dag2 (N.root dag1)

instance Monoid TopoDAG where
    mempty = TopoDAG mempty
    mappend (TopoDAG topo1) (TopoDAG topo2) = TopoDAG $ topo1 { TN.children = topo2 : TN.children topo1 }

instance SN.SubsettableNetwork DAG NodeInfo where
  appendSubtree = appendAt
  accessSubtree = grabAt

-- | This tree knows its edges
instance ET.EdgedTree DAG NodeInfo EdgeSet where
  edges    n t   = edges t ! code n
  setEdges n t e = t {edges = edges t // [(code n, e)]}

-- | This particular tree is referential
instance RT.ReferentialTree DAG NodeInfo where
  code node tree = elemIndex node (nodes tree)
  getNthNode tree pos = nodes tree ! pos

instance BinaryTree DAG NodeInfo where
    leftChild  n t = lookup 0 $ (\i -> nodes t ! i) <$> children n
    rightChild n t = lookup 1 $ (\i -> nodes t ! i) <$> children n
    verifyBinary   = all ((2 >=) . length . children) . nodes

instance RoseTree DAG NodeInfo where
    parent n t = headMay $ fmap (\i -> nodes t ! i) (parents n)

instance N.Network DAG NodeInfo where
    parents n t   = fmap (\i -> nodes t ! i) (parents n)
    root t        = nodes t ! root t
    children n t  = fmap (\i -> nodes t ! i) (children n)
    isLeaf n _    = isLeaf n
    isRoot n _    = isRoot n
    update t new  = t {nodes = nodes t // fmap (\n -> (code n, n)) new}
    numNodes      = length . nodes 
    addNode t n   = DAG nodes2 edges2 reroot
      where
          addPos = length $ nodes t
          newNode = resetPos n t addPos
          newEdge = makeEdges newNode t
          edges2 = edges t V.++ pure newEdge
          nodes2 = addConnections newNode (nodes t) V.++ pure newNode
          reroot = if isRoot n && null (nodes t) then addPos else root t

-- | Function to append two dags
appendAt :: DAG -> DAG -> NodeInfo -> DAG
appendAt d1@(DAG n e r) d2@(DAG n' e' r') hangNode
    | null n  = d2
    | null n' = d1
    | r > length n - 1 || r' > length n' - 1 = error "Root out of bounds when trying to append trees"
    | otherwise = DAG allNodes connectEdges r
        where
            shift = length n
            hCode = code hangNode
            -- hang and shift the nodes
            hungNodes = n' // [(r', (n' ! r') {isRoot = False, parents = [hCode]})]
            connectN = n // [(hCode, hangNode {children = (shift + r') : children hangNode, isLeaf = False})]
            recodeNew = fmap recodeFun hungNodes
            recodeFun m = m { code = code m + shift, children = fmap (shift +) (children m), parents = fmap (shift +) (parents m) }
            allNodes = connectN V.++ recodeNew
            -- update edges and add connecting edge
            reMapOut = IM.foldWithKey (\k val acc -> IM.insert (k + shift) (reMapInfo val) acc) mempty
            reMapInfo eInfo = eInfo {origin = allNodes ! (code (origin eInfo) + shift), terminal = allNodes ! (code (terminal eInfo) + shift)}
            shiftEdge edge = edge {inNodes = IS.map (shift +) (inNodes edge), outNodes = reMapOut (outNodes edge)}
            newEdges = fmap shiftEdge e'
            allEdges = e V.++ newEdges
            hangUpdate = (allEdges ! hCode) {outNodes = fmap (\info -> info {origin = allNodes ! hCode}) (outNodes (allEdges ! hCode))}
            hangAdd = hangUpdate <> EdgeSet (inNodes $ e ! hCode) (IM.insert (r' + shift) (EdgeInfo 0 (allNodes ! hCode) (allNodes ! (r' + shift)) Nothing) (outNodes $ e ! hCode))
            hangedUpdate = (allEdges ! (r' + shift)) <> EdgeSet (IS.singleton hCode) mempty
            connectEdges = allEdges // [(hCode, hangAdd), (r' + shift, hangedUpdate)]


-- | Function to grab from a DAG
grabAt :: DAG -> NodeInfo -> DAG
grabAt inTree hangNode = fromTopo rootedTopo
  where 
    topo = nodeToTopo inTree hangNode
    rootedTopo = TopoDAG topo

-- | Function to go from referential to topo
-- TODO define all these conversion functions
fromTopo :: TopoDAG -> DAG
fromTopo (TopoDAG inTopo) = internalFromTopo inTopo
    where
        internalFromTopo :: Topo -> DAG
        internalFromTopo topo
            | TN.isLeaf topo = singletonDAG topo 
            | otherwise = foldr (\n acc -> acc <> internalFromTopo n) (singletonDAG topo) (TN.children topo)
                where
                    -- | Function to convert a node to a tree for folding
                    singletonDAG :: Topo -> DAG
                    singletonDAG topoNode = 
                      let myNode = Node 0 (TN.name topoNode) (TN.isRoot topoNode) (TN.isLeaf topoNode) [] [] (TN.encoded topoNode) (TN.packed topoNode) (TN.preliminary topoNode) 
                                              (TN.final topoNode) (TN.temporary topoNode) (TN.aligned topoNode) mempty mempty mempty mempty (TN.localCost topoNode) (TN.totalCost topoNode)
                      in DAG (pure myNode) (pure mempty) 0 

-- | Function to go from topo to referential
toTopo :: DAG -> TopoDAG
toTopo tree = TopoDAG $ nodeToTopo tree (nodes tree ! root tree)

-- | convert a given node to topo
nodeToTopo :: DAG -> NodeInfo -> Topo
nodeToTopo inDAG curNode
    | isLeaf curNode = leaf
    | otherwise = leaf {TN.children = childDAGs}
      where
          childDAGs = fmap (\i -> nodeToTopo inDAG (nodes inDAG ! i)) (children curNode)
          leaf = TN.TopoNode (isRoot curNode) (isLeaf curNode) (name curNode) mempty (encoded curNode) (packed curNode) (preliminary curNode) 
                  (final curNode) (temporary curNode) (aligned curNode) (random curNode) (union curNode) (single curNode) (gapped curNode) (localCost curNode) (totalCost curNode)

-- | makeEdges is a small function assisting appendAt
-- it creates the edge set for a given node in the given tree
makeEdges :: NodeInfo -> DAG -> EdgeSet
makeEdges node inDAG = EdgeSet (IS.fromList $ parents node) out
  where
    out  = foldr (\i acc -> IM.insert i (info $ nodes inDAG ! i) acc) mempty (children node)
    info input = EdgeInfo 0 node input Nothing

-- | resetPos is a small function assisting the joining of two subtrees
-- simple function to reset positioning of a node
resetPos :: NodeInfo -> DAG -> Int -> NodeInfo
resetPos node prevDAG index =
  let
    leaf = null $ children node
    nroot = null (parents node) && null (nodes prevDAG)
  in node {code = index, isLeaf = leaf, isRoot = nroot}

-- | addConnections is a small function assiting subtree joins
-- it adds edges between a new node and an existing tree
addConnections :: NodeInfo -> Vector NodeInfo -> Vector NodeInfo
addConnections newNode myNodes = 
  let 
    setIn curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {children = code newNode : children (curNodes ! curPos), isLeaf = False})]
    withIn = foldr setIn myNodes (parents newNode)
    setOut curPos curNodes = curNodes // [(curPos, (curNodes ! curPos) {parents = code newNode : parents (curNodes ! curPos), isRoot = False})]
    withOut = foldr setOut withIn (children newNode)
  in withOut 

-- | Convert from a Newick format to a current DAG
fromNewick :: New.NewickForest -> Forest DAG
--fromNewick forest | trace ("fromNewick on forest " ++ show forest) False = undefined
fromNewick forest = fst $ foldr (\d (acc, counter) -> first (: acc) $ oneNewick counter d) ([], 0) forest
  where
    oneNewick :: Int -> New.NewickNode -> (DAG, Int)
    --oneNewick new | trace ("oneNewick on tree " ++ show new) False = undefined
    oneNewick count new = first fromTopo $ newickTopo count new
    
    newickTopo :: Int -> New.NewickNode -> (TopoDAG, Int)
    --newickTopo tree0 | trace ("newickTopo on tree " ++ show tree0) False = undefined
    newickTopo count tree0 = first (TopoDAG . (\x -> x {TN.isRoot = True})) $ internalNewick count tree0 
      where
        internalNewick :: Int -> New.NewickNode -> (Topo, Int)
        internalNewick nameCount inTree = (outNode, nextNameCount)
          where
            myName = fromMaybe ("HTU " ++ show nameCount) (New.newickLabel inTree)
            baseCase = ([], if isNothing $ New.newickLabel inTree then nameCount + 1 else nameCount)
            myCost = fromMaybe 0 (New.branchLength inTree)
            --recurse = V.toList $ V.imap (\i n -> internalNewick n (nameCount + i + 1)) (V.fromList $ New.descendants inTree) 
            (recurse,nextNameCount) = foldr (\n (acc,i) -> first (: acc) $ internalNewick i n) baseCase (New.descendants inTree) 
            outNode = TN.TopoNode False (null $ New.descendants inTree) myName recurse mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty myCost 0