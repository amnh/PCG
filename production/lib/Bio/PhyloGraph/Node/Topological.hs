-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Graph.Random
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

data TopoNode b = TopoNode
                    { isRoot       :: Bool
                    , isLeaf       :: Bool
                    , name         :: String
                    , children     :: [TopoNode b]
                    , encoded      :: Vector EncodedSeq -- encoded version of original assignment
                    , packed       :: Vector EncodedSeq -- packed version of the sequence
                    , preliminary  :: Vector EncodedSeq -- preliminary assignment at a node
                    , final        :: Vector EncodedSeq -- final assignment at a node
                    , temporary    :: Vector EncodedSeq -- multipurpose temporary assignment 
                    , aligned      :: Vector EncodedSeq -- the alignment between the children
                    , random       :: Vector EncodedSeq -- the assignment with a single state randomly selected to remove ambiguity
                    , union        :: Vector EncodedSeq -- the union assignment
                    , single       :: Vector EncodedSeq -- the single assignment
                    , gapped       :: Vector EncodedSeq -- the final assignment with gaps for alignment
                    , localCost    :: Double
                    , totalCost    :: Double
                    } deriving (Eq, Show)


-- | In a monoid instance, we take mappend to mean a joining of the two subtrees
-- where the second subtree passed becomes a child of the first
instance Monoid (TopoNode b) where
     mempty = TopoNode False False mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty 0 0
     mappend n1 n2 = n1 {children = n2 : children n1}
