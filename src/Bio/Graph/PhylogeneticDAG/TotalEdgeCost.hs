------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.TotalEdgeCost
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, FlexibleContexts, MonoLocalBinds #-}

module Bio.Graph.PhylogeneticDAG.TotalEdgeCost
  ( totalEdgeCosts
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Prelude            hiding (lookup, zipWith)


-- |
-- Computes the total edge cost over all the disambiguated final assignments.
totalEdgeCosts
  :: ( HasCharacterWeight z r
     , HasSingleDisambiguation z c
     , Integral i
     , NFData i
     , NFData r
     , Num r
     )
  => (c -> c -> (i, c, c, c, c))
  -> PhylogeneticDAG2 e n u v w x y z
  -> NonEmpty [r]
totalEdgeCosts pariwiseFunction (PDAG2 dag) = applyWeights $ foldlWithKey f initAcc refVec
  where
    refVec = references dag

    pariwiseFunction' lhs rhs = (\(!x,_,_,_,_) -> x) $ pariwiseFunction lhs rhs

    initAcc = fmap ((0 <$) . toList . dynamicCharacters) . getSequence . NE.head $ rootRefs dag

    getSequence = NE.fromList . otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)

    getFields = fmap (fmap (^. singleDisambiguation) . toList . dynamicCharacters) . getSequence

    weightSequence = fmap (fmap (^. characterWeight) . toList . dynamicCharacters) . getSequence . NE.head $ rootRefs dag

    applyWeights = force . zipWith (zipWith (\d w -> d * fromIntegral w)) weightSequence

    -- For each node in the DAG, fold over the connected edges that have not yet been traversed
    -- and accumulate the total edge cost of each dynamic character.
--    f :: NonEmpty [i] -> Int -> IndexData e n -> NonEmpty [i]
    f acc key node = ofoldl' g acc applicableNodes
      where
        adjacentNodes   = parentRefs node <> IM.keysSet (childRefs node)
        applicableNodes = IS.filter (> key) adjacentNodes
        nodeSequence    = getFields key

        -- Folding function for adjacent nodes. Should apply the sum strictly.
        g seqAcc = force . zipWith (zipWith (+)) seqAcc . zipWith (zipWith pariwiseFunction') nodeSequence . getFields
        
