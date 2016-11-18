-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.Node
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Bio.PhyloGraphPrime.Node (PhylogeneticNode (..)) where


import Data.Bifunctor

-- |
-- This serves as a computation invariant node decoration designed to hold node
-- information such as name and later a subtree structure.
data  PhylogeneticNode n s
    = PNode
    { nodeName           :: String
    , nodeDecoration     :: n
    , sequenceDecoration :: s
    } deriving (Eq, Functor)

instance Bifunctor PhylogeneticNode where

    bimap g f = 
      PNode <$> nodeName
            <*> g . nodeDecoration
            <*> f . sequenceDecoration
