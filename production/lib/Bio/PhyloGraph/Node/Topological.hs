-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.Topological
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module making a topological node similar to an ordinary node
-- allows for good random generation behavior
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.Topological (TopoNode(..)) where

import Bio.Sequence.Coded
import Data.Vector

-- | A tree construction which stores it's children as pointers. Tree traversal
--   must start from the root node.
data TopoNode b
   = TopoNode
   { isRoot       :: Bool
   , isLeaf       :: Bool
   , name         :: String
   , children     :: [TopoNode b]
   , encoded      :: Vector EncodedSeq -- | Encoded version of original assignment.
   , packed       :: Vector EncodedSeq -- | Packed version of the sequence.
   , preliminary  :: Vector EncodedSeq -- | Preliminary assignment at a node.
   , final        :: Vector EncodedSeq -- | Final assignment at a node.
   , temporary    :: Vector EncodedSeq -- | Multipurpose temporary assignment.
   , aligned      :: Vector EncodedSeq -- | The alignment between the children.
   , random       :: Vector EncodedSeq -- | The assignment with a single state randomly selected to remove ambiguity.
   , union        :: Vector EncodedSeq -- | The union assignment.
   , single       :: Vector EncodedSeq -- | The single assignment.
   , gapped       :: Vector EncodedSeq -- | The final assignment with gaps for alignment.
   , localCost    :: Double
   , totalCost    :: Double
   } deriving (Eq, Show)


-- | In a monoid instance, we take mappend to mean a joining of the two subtrees
-- where the second subtree passed becomes a child of the first
instance Monoid (TopoNode b) where
     mempty = TopoNode False False mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty 0 0
     mappend n1 n2 = n1 {children = n2 : children n1}
