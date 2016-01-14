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
import qualified Bio.Phylogeny.Tree.Node.Encoded as EN
import qualified Bio.Phylogeny.Tree.Node.Final as FN
import qualified Bio.Phylogeny.Tree.Node.Packed as PN
import qualified Bio.Phylogeny.Tree.Node.Preliminary as RN

import Data.Vector

data TopoNode b = TopoNode
                    { isRoot :: Bool
                    , isLeaf :: Bool
                    , name :: String
                    , children :: [TopoNode b]
                    , encoded :: EncodedSequences b
                    , packed :: EncodedSequences b
                    , preliminary :: EncodedSequences b
                    , final :: EncodedSequences b
                    , temporary :: EncodedSequences b
                    , aligned :: EncodedSequences b
                    , cost :: Double} deriving (Eq, Show)

data EdgeInfo = EdgeInfo {len :: Double} deriving (Eq, Show)