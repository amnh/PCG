-------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block
  ( CharacterBlock()
  , MetadataBlock()
  , HasBlockCost
  , HasRootCost
  -- * Cost Queries
  , blockCost
  , rootCost
  , staticCost
  -- * Transformations
  , toMissingCharacters
  , hexmap
  , hexTranspose
  , hexZipWith
  , hexZipWithMeta
  ) where

import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence.Block.Character
import           Bio.Sequence.Block.Internal
import           Bio.Sequence.Block.Metadata
import           Control.Arrow                        ((***))
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Key
import           Data.Vector.Instances                ()
import           Prelude                       hiding (zip)

import           Debug.Trace


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z =
    ( HasCharacterCost   u Double
    , HasCharacterCost   v Word
    , HasCharacterCost   w Word
    , HasCharacterCost   x Word
    , HasCharacterCost   y Word
    , HasCharacterCost   z Word
    )


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasRootCost u v w x y z =
    ( HasAverageLength z AverageLength
    , PossiblyMissingCharacter u
    , PossiblyMissingCharacter v
    , PossiblyMissingCharacter w
    , PossiblyMissingCharacter x
    , PossiblyMissingCharacter y
    , PossiblyMissingCharacter z
    )


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
blockCost :: HasBlockCost u v w x y z => MetadataBlock e d m -> CharacterBlock u v w x y z -> Double
blockCost (MB mBlock) (CB cBlock) = sum . fmap sum $
    [ parmap rpar floatingCost . uncurry zip . ( continuousBins ***  continuousBins) 
    , parmap rpar integralCost . uncurry zip . (nonAdditiveBins *** nonAdditiveBins)
    , parmap rpar integralCost . uncurry zip . (   additiveBins ***    additiveBins)   
    , parmap rpar integralCost . uncurry zip . (     metricBins ***      metricBins)
    , parmap rpar integralCost . uncurry zip . (  nonMetricBins ***   nonMetricBins)  
    , parmap rpar (uncurry (*)) {- . traceShowId -} . fmap ((^. characterWeight) *** fromIntegral . (^. characterCost)) . uncurry zip . (    dynamicBins ***     dynamicBins)
    ] <*> [(mBlock, cBlock)]
  where
    integralCost (m, _) | (m ^. characterWeight :: Double) == 0 && trace (show "Weight of 0 found") False = undefined
    integralCost (m, c) = fromIntegral cost * weight
      where
        cost   = c ^. characterCost
        weight = m ^. characterWeight

    floatingCost (m, c) = cost * weight
      where
        cost   = c ^. characterCost
        weight = m ^. characterWeight


-- |
-- Calculate the "rooting cost" of a 'CharacterBlock' by applying a "rooting-
-- multiplier" based on the number of other roots in the DAG.
rootCost
  :: ( HasRootCost u v w x y z
     , Integral i
     )
  => i
  -> MetadataBlock e d m
  -> CharacterBlock u v w x y z
  -> Double
rootCost rootCount (MB mBlock) (CB cBlock) = rootMultiplier . sum . fmap sum $
    [ parmap rpar staticRootCost  . uncurry zip . ( continuousBins ***  continuousBins)
    , parmap rpar staticRootCost  . uncurry zip . (nonAdditiveBins *** nonAdditiveBins)
    , parmap rpar staticRootCost  . uncurry zip . (   additiveBins ***    additiveBins)
    , parmap rpar staticRootCost  . uncurry zip . (     metricBins ***      metricBins)
    , parmap rpar staticRootCost  . uncurry zip . (  nonMetricBins ***   nonMetricBins)
    , parmap rpar dynamicRootCost . uncurry zip . (    dynamicBins ***     dynamicBins)
    ] <*> [(mBlock, cBlock)]
  where
    rootMultiplier x = (otherRoots * x) / 2
      where
        otherRoots = max 0 (fromIntegral rootCount - 1)
    
    staticRootCost (m, c)
      | isMissing c = 0
      | otherwise   = m ^. characterWeight

    dynamicRootCost (m, c)
      | isMissing c = 0
      | otherwise   = weight * getAverageLength avgLen
      where
        avgLen = c ^. averageLength
        weight = m ^. characterWeight


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
staticCost :: HasBlockCost u v w x y z => MetadataBlock e d m -> CharacterBlock u v w x y z -> Double
staticCost (MB mBlock) (CB cBlock) = sum . fmap sum $
    [ parmap rpar floatingCost . uncurry zip . ( continuousBins ***  continuousBins) 
    , parmap rpar integralCost . uncurry zip . (nonAdditiveBins *** nonAdditiveBins)
    , parmap rpar integralCost . uncurry zip . (   additiveBins ***    additiveBins)   
    , parmap rpar integralCost . uncurry zip . (     metricBins ***      metricBins)
    , parmap rpar integralCost . uncurry zip . (  nonMetricBins ***   nonMetricBins)  
    ] <*> [(mBlock, cBlock)]
  where
    integralCost (m, c) = fromIntegral cost * weight
      where
        cost   = c ^. characterCost
        weight = m ^. characterWeight

    floatingCost (m, c) = cost * weight
      where
        cost   = c ^. characterCost
        weight = m ^. characterWeight
