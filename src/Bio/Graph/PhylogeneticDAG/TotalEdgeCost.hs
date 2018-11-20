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

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeFamilies     #-}

module Bio.Graph.PhylogeneticDAG.TotalEdgeCost
  ( totalEdgeCosts
  ) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import qualified Data.IntMap                                   as IM
import qualified Data.IntSet                                   as IS
import           Data.Key
import           Data.List.NonEmpty                            (NonEmpty (..))
import qualified Data.List.NonEmpty                            as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Prelude                                       hiding (zipWith)

--import Debug.Trace


-- |
-- Computes the total edge cost over all the disambiguated final assignments.
totalEdgeCosts
  :: ( EncodableDynamicCharacter c
     , Exportable c
     , Exportable (Element c)
     , HasSingleDisambiguation       z c
     , Ord (Element c)
     , Element c ~ DynamicCharacterElement
     )
  => PhylogeneticDAG2 m e n u v w x y z
  -> NonEmpty [Double]
--totalEdgeCosts _ (PDAG2 dag _) | trace ("Before Total Edge Cost: " <> referenceRendering dag) False = undefined
totalEdgeCosts (PDAG2 dag meta) = {-- toNonEmpty . --} applyWeights $ foldlWithKey f initAcc refVec
  where
    refVec  = references dag

    roots   = rootRefs dag

    initAcc = (0 <$) . toList . (^. dynamicBin) <$> sequencesWLOG

    sequencesWLOG = getSequence $ NE.head roots

    dynamicMetadataSeq = toList . (^. dynamicBin) <$> toNonEmpty (meta ^. blockSequence)

    getSequence = NE.fromList . otoList . characterSequence . NE.head . resolutions . nodeDecoration . (refVec !)

    getFields        = fmap (fmap (^. singleDisambiguation) . toList . (^. dynamicBin)) . getSequence

    weightSequence   = fmap (^. characterWeight) <$> dynamicMetadataSeq

    functionSequence = fmap getDynamicMetric <$> dynamicMetadataSeq
      where
        getDynamicMetric m x y = let (!c,_,_,_,_) = selectDynamicMetric m x y
                                 in {- trace ("Cost " <> show c) -} c

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
              case filter (/= key) . IM.keys . childRefs $ refVec ! i of
                x:_ -> x
                _   -> error $ unlines
                    [ "Found the empty list after filtering the child references which are not equal to the key."
                    , "The key we are considering is: " <> show key
                    , "The child index was: " <> show i
                    , "The child references where: " <> show (IM.keys . childRefs $ refVec ! i)
                    ]

