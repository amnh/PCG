<<<<<<< HEAD
-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exploritory types for Graph representations
--
-----------------------------------------------------------------------------
module Bio.Phylogeny.Graph where

import Data.IntMap
import Data.IntSet
import Data.HashMap.Strict
import Data.Monoid
import Data.Vector

type Identifier = String
type Sequence   = Vector [String]
type CharInfo   = String
type NodeInfo   = String

data EdgeSet
   = EdgeSet
   { parents  :: IntSet
   , children :: IntSet
   } deriving (Eq,Show)

data Graph
   = Graph
   { taxaNodes  :: IntMap  Identifier
   , taxaSeqs   :: HashMap Identifier Sequence
   , characters :: Vector  CharInfo
   , nodes      :: Vector  NodeInfo
   , edges      :: Vector  EdgeSet
   , roots      :: IntSet
   } deriving (Eq,Show)

-- Seems like this could be derived... silly GHC
instance Monoid Graph where
  mempty = Graph mempty mempty mempty mempty mempty mempty
  mappend (Graph a b c d e f) (Graph a' b' c' d' e' f') = Graph (a<>a') (b<>b') (c<>c') (d<>d') (e<>e') (f<>f')

