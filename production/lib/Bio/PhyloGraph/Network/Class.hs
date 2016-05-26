-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Network.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class with operations on a network and a node inside it
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.PhyloGraph.Network.Class where

{- | Type class Laws:

     nodeIsRoot (root t) t
     (numNodes t) > 1 ===> not (nodeIsLeaf (root t) t)
     forall a. (root t) /= a ===> not (nodeIsRoot a t)
     forall a. null (parents  (nodeIsRoot a t) t)
     forall a. null (children (nodeIsLeaf a t) t)
     numNodes (addNode t a) == numNodes t + 1
     numNodes (update  t a) == numNodes t

 -}

-- | A network holds topological information as well as the ability to update based on a list of nodes
class Network t n | t -> n where
    parents    :: n -> t -> [n] 
    children   :: n -> t -> [n]
    root       :: t -> n
    numNodes   :: t -> Int
    -- TODO: Redife this to be safe
    update     :: t -> [n] -> t
    -- TODO: Not used, redefine
    addNode    :: t -> n -> t
    nodeIsLeaf, nodeIsRoot :: n -> t -> Bool
    nodeIsLeaf n t = null $ children n t 
    nodeIsRoot n t = null $ parents  n t
    
    -- TODO add tree breaking at node, delete node, cut tree at condition, delete edge, delete edges, add edge
