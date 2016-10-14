-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ReferenceDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.PhyloGraphPrime.ReferenceDAG.Internal where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.IntMap        (IntMap)
import Data.IntSet        (IntSet)
import Data.Vector        (Vector)

-- |
-- A constant time access representation of a directed acyclic graph.
-- 
data ReferenceDAG e n
   = RefDAG
   { references :: Vector (n, IntSet, IntMap e)
   , rootRefs   :: NonEmpty Int
   , graphData  :: GraphData
   }


data GraphData
   = GraphData
   { cost :: Double
   }


instance Bifunctor ReferenceDAG where

    bimap f g dag =
        RefDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (node, parentRefs, childRefs) = (g node, parentRefs, f <$> childRefs)


instance Functor (ReferenceDAG e) where

    fmap f dag =
        RefDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (node, parentRefs, childRefs) = (f node, parentRefs, childRefs)
