-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EdgeSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Set-like structures for collection of edges.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.EdgeSet
  ( EdgeSet()
  , makeParentEdgeSet
  , singletonEdgeSet
  ) where

import           Control.DeepSeq
import           Data.Binary
import           Data.Coerce
import           Data.Foldable
import           Data.IntSet          (IntSet)
import           Data.MonoTraversable (MonoFoldable (..))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           GHC.Generics         (Generic)
import           TextShow             (TextShow)
import           TextShow.Instances   ()


-- |
-- Represents a collection of edges.
--
-- Often used to represent a spanning tree in a DAG.
newtype EdgeSet e = ES (Set e)
    deriving anyclass (Binary)
    deriving stock    (Eq, Generic, Ord)
    deriving newtype  (Foldable, Monoid, Semigroup, TextShow)


instance NFData e => NFData (EdgeSet e) where

    rnf (ES set) = rnf set


instance Show a => Show (EdgeSet a) where

  show (ES xs) = show (toList xs)


-- |
-- Construct a singleton 'EdgeSet' value. Use the semigroup operator '(<>)' to
-- construct a larger 'EdgeSet'. This enforces the non-empty invariant of the
-- 'EdgeSet' data structure.
singletonEdgeSet :: e -> EdgeSet e
singletonEdgeSet = ES . Set.singleton


-- |
-- Take node index and the parent indices and add the edge from parent to node.
-- This is only intended to be used on non-root nodes.
makeParentEdgeSet
  :: Int                -- ^ Current node index
  -> IntSet             -- ^ Parent indices
  -> EdgeSet (Int, Int)
makeParentEdgeSet currInd = ofoldMap (\parInd -> singletonEdgeSet (parInd, currInd))
