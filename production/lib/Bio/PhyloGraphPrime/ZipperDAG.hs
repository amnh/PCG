-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ZipperDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Phylogentic Graph types.
--
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Bio.PhyloGraphPrime.ZipperDAG where

import Data.Tree
import Data.Key
import Data.List.NonEmpty            (NonEmpty)
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Prelude                hiding (lookup)


-- |
-- We assert that the graph is a DAG
data ZipperNode e n = ZNode n [ZipperEdge n e]

data ZipperEdge n e = ZEdge EdgeDirection e (ZipperNode e n) (ZipperNode e n)

data EdgeDirection = Leftward | Rightward

-- TODO: Make a nice monadic rendering here with depth and stacks.
{-
instance (Show e, Show n) =>  Show (ZipperNode e a) where

    show ZNode n [ZipperEdge n e]
-}
