-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.EdgeSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Data.EdgeSet
  ( EdgeSet()
  , NetworkDisplayEdgeSet(..)
  , SetLike(..)
  , collapseToEdgeSet
  , fromEdgeSets
  , singletonEdgeSet
  ) where


import           Control.DeepSeq 
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set           (Set)
import qualified Data.Set    as Set
import           GHC.Generics       (Generic)
import           Prelude     hiding (zipWith)


newtype EdgeSet e = ES (Set e)
  deriving (Foldable, Generic, Monoid, Semigroup, Show)


newtype NetworkDisplayEdgeSet e = NDES (NonEmpty (EdgeSet e))
  deriving (Generic, Show)


class SetLike s where

    union :: s -> s -> s

    intersection :: s -> s -> s

    difference :: s -> s -> s

    cardinality :: s -> Word


instance NFData e => NFData (EdgeSet e) where

    rnf (ES set) = rnf set


instance NFData e => NFData (NetworkDisplayEdgeSet e) where

    rnf (NDES sets) = rnf sets


instance Ord a => SetLike (Set a) where

    union        = Set.union

    intersection = Set.intersection

    difference   = Set.difference

    cardinality  = fromIntegral . length 


instance Ord a => SetLike (EdgeSet a) where

    union        (ES x) (ES y) = ES $ union x y 

    intersection (ES x) (ES y) = ES $ intersection x y 

    difference   (ES x) (ES y) = ES $ difference x y 

    cardinality  (ES x) = cardinality x


instance Ord a => SetLike (NetworkDisplayEdgeSet a) where

    union        (NDES x) (NDES y) = NDES $ zipWith union x y 

    intersection (NDES x) (NDES y) = NDES $ zipWith intersection x y 

    difference   (NDES x) (NDES y) = NDES $ zipWith difference x y 

    cardinality  (NDES x) = sum $ cardinality <$> x


instance Ord a => Semigroup (NetworkDisplayEdgeSet a) where

    (NDES x) <> (NDES y) = NDES $ zipWith (<>) x y


collapseToEdgeSet :: Ord e => NetworkDisplayEdgeSet e -> EdgeSet e
collapseToEdgeSet (NDES x) = fold1 x


fromEdgeSets :: NonEmpty (EdgeSet e) -> NetworkDisplayEdgeSet e
fromEdgeSets = NDES


singletonEdgeSet :: e -> EdgeSet e
singletonEdgeSet = ES . Set.singleton
