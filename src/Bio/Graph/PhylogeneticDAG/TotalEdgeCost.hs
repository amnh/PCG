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


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
-- import           Bio.Character.Encodable.Stream
import           Bio.Sequence
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State.Lazy
-- import qualified Data.Alphabet      as A
import           Data.Foldable
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Prelude            hiding (lookup, zipWith)

import Debug.Trace


-- |
-- Computes the total edge cost over all the disambiguated final assignments.
totalEdgeCosts
  :: ( HasCharacterWeight z r
     , HasSingleDisambiguation z c
     , HasSymbolChangeMatrix z (Word -> Word -> Word)
     , Integral i
     , NFData i
     , NFData r
     , Num r
     )
  => (c -> c -> (Word -> Word -> Word) -> (i, c, c, c, c))
  -> PhylogeneticDAG2 e n u v w x y z
  -> NonEmpty [r]
totalEdgeCosts pariwiseFunction (PDAG2 dag) = applyWeights $ foldlWithKey f initAcc refVec
  where
    refVec = references dag

    roots  = rootRefs dag

    pariwiseFunction' lhs rhs tcm = (\(!x,_,_,_,_) -> x) $ pariwiseFunction lhs rhs tcm

    initAcc = ((0 <$) . toList . dynamicCharacters) <$> sequencesWLOG

    sequencesWLOG = getSequence $ NE.head roots

    getSequence = NE.fromList . otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)

    getFields = fmap (fmap (^. singleDisambiguation) . toList . dynamicCharacters) . getSequence

    weightSequence = (fmap (^. characterWeight) . toList . dynamicCharacters) <$> sequencesWLOG

    tcmSequence = (fmap (^. symbolChangeMatrix) . toList . dynamicCharacters) <$> sequencesWLOG

    functionSequence = (fmap (\tcm x y -> pariwiseFunction' x y tcm)) <$> tcmSequence 

--    showChar = showStream alphabet

--    alphabet = A.fromSymbols ["A","C","G","T"]

    applyWeights = force . zipWith (zipWith (\d w -> d * fromIntegral w)) weightSequence

    -- For each node in the DAG, fold over the connected edges that have not yet been traversed
    -- and accumulate the total edge cost of each dynamic character.
--    f :: NonEmpty [i] -> Int -> IndexData e n -> NonEmpty [i]
    f acc key node
      | key `elem` roots = acc
      | otherwise = ofoldl' g acc applicableNodes
      where
        adjacentNodes   = IS.map collapseRootEdge $ parentRefs node <> IM.keysSet (childRefs node)
        applicableNodes = IS.map (\x -> trace (show (key, x)) x) $ IS.filter (> key) adjacentNodes
        nodeSequence    = getFields key

        -- Folding function for adjacent nodes. Should apply the sum strictly.
--        g seqAcc = force . zipWith (zipWith (+)) seqAcc . zipWith (zipWith pariwiseFunction') . nodeSequence . getFields
        g seqAcc = force . zipWith (zipWith (+)) seqAcc .
                           zipWith (zipWith ($)) (zipWith (zipWith ($)) functionSequence nodeSequence) . getFields
        
        collapseRootEdge i
          | i `notElem` roots = i
          | otherwise = (\x-> trace (unwords [ show i
                                             , " is a root index, so the edge"
                                             , show (i, key)
                                             , "is invalid!"
                                             , show x
                                             , "is the indicen node index, so the replacement edge is"
                                             , show (x, key)
                                             ]) x) .
                        head . filter (/= key) .  IM.keys . childRefs $ refVec ! i
            
