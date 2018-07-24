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


import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Sequence
import           Bio.Sequence.Metadata        (getDynamicMetadata)
import qualified Bio.Sequence.Metadata as M
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
--import           Data.TCM.Memoized
import           Prelude            hiding (zipWith)

--import Debug.Trace


-- |
-- Computes the total edge cost over all the disambiguated final assignments.
totalEdgeCosts
  :: ( EncodableDynamicCharacter c
     , Exportable c
     , Exportable (Element c)
--     , HasCharacterWeight            z r
--     , HasDenseTransitionCostMatrix  z (Maybe DenseTransitionCostMatrix)
     , HasSingleDisambiguation       z c
--     , HasSparseTransitionCostMatrix z MemoizedCostMatrix
--     , Integral i
--     , Show i
--     , NFData i
--     , NFData r
--     , Num r
     , Ord (Element c)
     )
  => PhylogeneticDAG2 m a d e n u v w x y z
  -> NonEmpty [Double]
--totalEdgeCosts _ (PDAG2 dag _) | trace ("Before Total Edge Cost: " <> referenceRendering dag) False = undefined
totalEdgeCosts (PDAG2 dag meta) = applyWeights $ foldlWithKey f initAcc refVec
  where
    refVec = references dag

    roots  = rootRefs dag

--    pariwiseFunction' lhs rhs tcm = (\(!x,_,_,_,_) -> {- trace ("Cost " <> show x) -} x) $ pariwiseFunction lhs rhs tcm

    initAcc = (0 <$) . toList . dynamicCharacters <$> sequencesWLOG

    sequencesWLOG = getSequence $ NE.head roots

    dynamicMetadataSeq = toList . getDynamicMetadata <$> M.toBlocks meta

    getSequence = NE.fromList . otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)

    getFields = fmap (fmap (^. singleDisambiguation) . toList . dynamicCharacters) . getSequence

    weightSequence = fmap (^. characterWeight) <$> dynamicMetadataSeq

--    tcmSequence = (fmap (^. symbolChangeMatrix) . toList . dynamicCharacters) <$> sequencesWLOG
--    functionSequence = fmap (\tcm x y -> pariwiseFunction' x y tcm) <$> tcmSequence 

    functionSequence = fmap getDynamicMetric <$> dynamicMetadataSeq
      where
        getDynamicMetric dec x y = let (!c,_,_,_,_) = selectDynamicMetric dec x y in {- trace ("Cost " <> show c) -} c

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
        adjacentNodes   = IS.map collapseRootEdge $
                              parentRefs node <>
                              IM.keysSet (childRefs node)
        applicableNodes = {- IS.map (\x -> trace ("Edge: " <> show (key, x)) x) $ -} IS.filter (> key) adjacentNodes
        nodeSequence    = getFields key

        -- Folding function for adjacent nodes. Should apply the sum strictly.
--        g seqAcc = force . zipWith (zipWith (+)) seqAcc . zipWith (zipWith pariwiseFunction') . nodeSequence . getFields
        g seqAcc = force . zipWith (zipWith (+)) seqAcc .
                           zipWith (zipWith ($)) (zipWith (zipWith ($)) functionSequence nodeSequence) . getFields
        
        collapseRootEdge i
          | i `notElem` roots = i
          | otherwise = {- (\x-> trace (unwords [ show i
                                             , " is a root index, so the edge"
                                             , show (i, key)
                                             , "is invalid!"
                                             , show x
                                             , "is the incident node index, so the replacement edge is"
                                             , show (x, key)
                                             ]) x) .
                        -}
                        head . filter (/= key) .  IM.keys . childRefs $ refVec ! i
            
