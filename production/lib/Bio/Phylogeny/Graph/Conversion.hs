-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Conversion
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Conversion functionality for a Graph
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph.Conversion where

import              Bio.Phylogeny.Graph
import qualified    Bio.Phylogeny.Graph.Topological     as TG
import qualified    Bio.Phylogeny.Network               as NW
import              Bio.Phylogeny.Tree.Node      
import qualified    Bio.Phylogeny.Tree.Node.Topological as TN

import              Data.Vector                             ((!))
import qualified    Data.IntMap                         as IM
import              Data.Monoid

fromTopo :: TG.TopoTree -> Tree
fromTopo topo
    | TN.isLeaf topo = myTree
    | otherwise = foldr (\n acc -> acc <> fromTopo n) myTree (TN.children topo)

        where
            myNode = Node 0 (TN.isRoot topo) (TN.isLeaf topo) [] [] (TN.encoded topo) (TN.packed topo) (TN.preliminary topo) 
                        (TN.final topo) (TN.temporary topo) (TN.aligned topo) (TN.cost topo)
            myTree = mempty `NW.addNode` myNode

toTopo :: Tree -> TG.TopoTree
toTopo tree = nodeToTopo tree (nodes tree ! root tree)
    where
        nodeToTopo :: Tree -> NodeInfo -> TG.TopoTree
        nodeToTopo inTree curNode
            | isLeaf curNode = leaf
            | otherwise = 
                let childTrees = map (\i -> nodeToTopo inTree (nodes tree ! i)) (children curNode)
                in leaf {TN.children = childTrees}
                where
                    leaf = TN.TopoNode (isRoot curNode) True (nodeNames tree IM.! (code curNode)) [] (encoded curNode) (packed curNode) (preliminary curNode) 
                            (final curNode) (temporary curNode) (aligned curNode) (cost curNode)
