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

{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Bio.PhyloGraphPrime.Node
  ( PhylogeneticNode (..)
  , PhylogeneticNode2(..)
  , ResolutionInformation(..)
  ) where


import Data.Bifunctor
import Data.BitVector
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup


-- |
-- This serves as a computation invariant node decoration designed to hold node
-- information such as name and later a subtree structure.
data  PhylogeneticNode n s
    = PNode
    { nodeDecorationDatum :: n
    , sequenceDecoration  :: s
    } deriving (Eq, Functor)


data  PhylogeneticNode2 n s
    = PNode2
    { resolutions          :: NonEmpty (ResolutionInformation s)
    , nodeDecorationDatum2 :: n
    } deriving (Eq, Functor)


data  ResolutionInformation s
    = ResInfo
    { leafSetRepresentation :: SubtreeLeafSet
    , subtreeRepresentation :: NewickSerialization
    , characterSequence     :: s
    , localSequenceCost     :: Double
    , totalSubtreeCost      :: Double 
    } deriving (Eq, Functor)


newtype SubtreeLeafSet = LS BitVector
  deriving (Eq, Bits)


newtype NewickSerialization = NS String
  deriving (Eq, Semigroup)


instance Bifunctor PhylogeneticNode where

    bimap g f = 
      PNode <$> g . nodeDecorationDatum
            <*> f . sequenceDecoration


instance Bifunctor PhylogeneticNode2 where

    bimap g f = 
      PNode2 <$> fmap (fmap f) . resolutions
             <*> g . nodeDecorationDatum2
