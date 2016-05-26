-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.DAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a DAG
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.DAG
  ( StandardDAG(..)
  , NodeInfo
  , Topo
  , DAG(..)
  , TopoDAG(..)
  , fromNewick
  , fromTopo
  , toTopo
  , arbitraryDAGGS
  ) where

