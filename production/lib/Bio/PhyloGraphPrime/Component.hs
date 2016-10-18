-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.Component
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.PhyloGraphPrime.Component where

import Data.List.NonEmpty

-- newtype PhylogeneticComponent a = PhylogeneticComponent a

{- Laws:

 Node queries are mutually exclusive.
 forall i, t . (==1) . length . filter id $
               [ i `isComponentNode` t
               , i `isNetworkNode` t
               , i `isTreeNode` t
               , i `isLeafNode` t
               , i `isRootNode` t
               ]
-}
-- |
-- Represents the most relaxed phylogentic graph structure.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * the graph contains one or more root nodes
--  * All nodes have at most in-degree 2
--  * All nodes have out-degree 0 or out-degree 2
class PhylogeneticComponent t i e n | t -> i, t -> n, t -> e where

    parents   :: i -> t -> [i]

    children  :: i -> t -> [i]

    roots     :: t -> NonEmpty i

    leaves    :: t -> [i]

    nodeCount :: t -> Int

    nodeDatum :: i -> t -> n

    edgeDatum :: (i,i) -> t -> Maybe e

    -- |
    -- A node satisfying:
    --  * In-degree  of 2 or more
    --  * Out-degree of 2 or more
    --  * A least 2 parent nodes have ancestoral paths to different root nodes
    isComponentNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree  of 2 or more
    --  * Out-degree of 2 or more
    --  * All parent nodes have ancestoral paths to a single root node
    isNetworkNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree  of 1
    --  * Out-degree of 2
    isTreeNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * Out-degree 0
    isLeafNode :: i -> t -> Bool

    -- |
    -- A node satisfying:
    --  * In-degree 0
    isRootNode :: i -> t -> Bool

    -- |
    -- Performs a softwire resolution of all /component/ nodes into a collection
    -- of all resulting networks. The resulting size of the collection is equal
    -- to /2^n/ where /n/ is the number of component nodes in the
    -- 'PhylogeneticComponent'.
    networkResolutions :: t -> NonEmpty t


-- |
-- Represents a more constrained phylogentic graph structure.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * The graph contains /exactly one/ root nodes
--  * All nodes have at most in-degree 2
--  * All nodes have out-degree 0 or out-degree 2
class PhylogeneticNetwork t i e n | t -> i, t -> n, t -> e where

    root  :: t -> i

    -- |
    -- Performs a softwire resolution of all /network/ nodes into a collection
    -- of all resulting trees. The resulting size of the collection is equal
    -- to /2^n/ where /n/ is the number of network nodes in the
    -- 'PhylogenetiNetwork'.
    treeResolutions :: t -> NonEmpty t


-- |
-- Represents the most constrained phylogentic graph structure.
-- The constraints correlate to a binary tree.
--
-- The graph must satisfy the following:
--  * The graph is directed
--  * The graph is acyclic
--  * The graph contains /exactly one/ root nodes
--  * All nodes have /exactly/ in-degree 1
--  * All nodes contain out-degree 0 or out-degree 2
class PhylogeneticTree t i e n | t -> i, t -> n, t -> e where

    parent :: i -> t -> Maybe i

