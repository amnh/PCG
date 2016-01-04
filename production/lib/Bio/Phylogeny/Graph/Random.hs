-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph.Random
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module making Graph an instance of Arbitrary for testing
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Graph.Random where

import Test.Tasty.QuickCheck

import Prelude hiding (filter)

import Debug.Trace

import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Topological
import Bio.Phylogeny.Tree.Node.Topological
import qualified Bio.Phylogeny.Tree.Node as N
import qualified Bio.Phylogeny.Network as NW

import Data.Maybe
import Data.Monoid
import Data.Vector (filter, toList)

instance Arbitrary Graph where
    arbitrary = Graph <$> listOf (arbitrary :: Gen Tree)

instance Arbitrary Tree where
    arbitrary = do
        topo <- arbitrary :: Gen TopoTree
        return $ convertTopo topo 0 Nothing

convertTopo :: TopoTree -> Int -> Maybe Int -> Tree
--convertTopo topo numNodes parentCode | trace ("Conversion from topo to normal " ++ show topo) False = undefined
convertTopo topo numNodes parentCode
    | isLeaf topo = 
        let myNode = N.Node curCode (isRoot topo) True [] (maybeToList parentCode) (encoded topo) (packed topo) (preliminary topo) (final topo) (temporary topo) (aligned topo) (cost topo)
        in mempty `NW.addNode` myNode
    | otherwise = 
        let 
            childTrees = foldr (\n acc -> convertTopo n curCode (Just curCode) <> acc) mempty (children topo)
            myChildren = toList $ filter (\n -> curCode == N.code n) (nodes childTrees)
            childCodes = map N.code myChildren
            myNode = N.Node curCode (isRoot topo) False childCodes (maybeToList parentCode) (encoded topo) (packed topo) (preliminary topo) (final topo) (temporary topo) (aligned topo) (cost topo)
        in childTrees `NW.addNode` myNode

        where curCode = numNodes + 1
