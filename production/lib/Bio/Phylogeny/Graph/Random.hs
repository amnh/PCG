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

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.Graph.Topological
import qualified Bio.Phylogeny.Tree.Node as N
import           Bio.Phylogeny.Tree.Node.Topological
import qualified Bio.Phylogeny.Network   as NW

import           Data.Maybe
import           Data.Monoid
import           Data.Vector    (filter, toList)

import           Prelude hiding (filter)
import           Test.Tasty.QuickCheck

--import Debug.Trace

instance Arbitrary Graph where
    arbitrary = Graph <$> listOf (arbitrary :: Gen Tree)

instance Arbitrary Tree where
    arbitrary = do
        topo <- arbitrary :: Gen TopoTree
        return $ convertTopo topo

convertTopo :: TopoTree -> Tree
convertTopo topo
    | isLeaf topo = myTree
    | otherwise = foldr (\n acc -> acc <> convertTopo n) myTree (children topo)

        where
            myNode = N.Node 0 (isRoot topo) (isLeaf topo) [] [] (encoded topo) (packed topo) (preliminary topo) (final topo) (temporary topo) (aligned topo) (cost topo)
            myTree = mempty `NW.addNode` myNode

