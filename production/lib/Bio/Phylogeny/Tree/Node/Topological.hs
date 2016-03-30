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
-- Module making a topological node similar to an ordinary node
-- allows for good random generation behavior
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Phylogeny.Tree.Node.Topological (TopoNode(..)) where

import Bio.Sequence.Coded
import Bio.Sequence.Parsed

data TopoNode b = TopoNode
                    { isRoot :: Bool
                    , isLeaf :: Bool
                    , name :: String
                    , parsed :: ParsedSequences
                    , children :: [TopoNode b]
                    , encoded :: EncodedSequences
                    , packed :: EncodedSequences
                    , preliminary :: EncodedSequences
                    , final :: EncodedSequences
                    , temporary :: EncodedSequences
                    , aligned :: EncodedSequences
                    , localCost :: Double
                    , totalCost :: Double} deriving (Eq, Show)

-- data EdgeInfo = EdgeInfo {len :: Double} deriving (Eq, Show)

-- | In a monoid instance, we take mappend to mean a joining of the two subtrees
-- where the second subtree passed becomes a child of the first
instance Monoid (TopoNode b) where
     mempty = TopoNode False False mempty mempty mempty mempty mempty mempty mempty mempty mempty 0 0
     mappend n1 n2 = n1 {children = n2 : children n1}