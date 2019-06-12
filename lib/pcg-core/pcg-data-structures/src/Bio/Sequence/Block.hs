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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Sequence.Block
  ( CharacterBlock()
  , MetadataBlock()
  , HasBlockCost
  , HasRootCost
  -- * Lenses
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
  -- * Cost Queries
  , blockCost
  , rootCost
  , staticCost
  -- * Transformations
  , toMissingCharacters
  , hexFold
  , hexmap
  , hexTranspose
  , hexZipMeta
  , hexZipWith
  , hexZipWith3
  , hexZipWithMeta
  , hexZip2WithMeta
  ) where

import Bio.Character.Decoration.Continuous
import Bio.Character.Decoration.Dynamic
import Bio.Sequence.Block.Character
import Bio.Sequence.Block.Metadata
import Control.Arrow                       ((***))
import Control.Lens
import Control.Parallel.Custom
import Control.Parallel.Strategies
import Data.Foldable
import Data.Foldable.Custom                (foldMap')
import Data.Key
import Data.Monoid                         (Sum (..))
import Data.Vector.Instances               ()
import Prelude                             hiding (zip)


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z =
    ( HasCharacterCost u Double
    , HasCharacterCost v Word
    , HasCharacterCost w Word
    , HasCharacterCost x Word
    , HasCharacterCost y Word
    , HasCharacterCost z Word
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
blockCost :: HasBlockCost u v w x y z => MetadataBlock m -> CharacterBlock u v w x y z -> Double
blockCost mBlock cBlock = nestedSum $
    [ parmap rpar floatingCost . uncurry zip . ((^.  continuousBin) *** (^.  continuousBin))
    , parmap rpar integralCost . uncurry zip . ((^. nonAdditiveBin) *** (^. nonAdditiveBin))
    , parmap rpar integralCost . uncurry zip . ((^.    additiveBin) *** (^.    additiveBin))
    , parmap rpar integralCost . uncurry zip . ((^.      metricBin) *** (^.      metricBin))
    , parmap rpar integralCost . uncurry zip . ((^.   nonMetricBin) *** (^.   nonMetricBin))
    , parmap rpar integralCost . uncurry zip . ((^.     dynamicBin) *** (^.     dynamicBin))
--    , parmap rpar (uncurry (*)) {- . traceShowId -} . fmap ((^. characterWeight) *** fromIntegral . (^. characterCost)) . uncurry zip . (    dynamicBins ***     dynamicBins)
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


-- |
-- Calculate the "rooting cost" of a 'CharacterBlock' by applying a "rooting-
-- multiplier" based on the number of other roots in the DAG.
rootCost
  :: ( HasRootCost u v w x y z
     , Integral i
     )
  => i
  -> MetadataBlock m
  -> CharacterBlock u v w x y z
  -> Double
rootCost rootCount mBlock cBlock = rootMultiplier . nestedSum $
    [ parmap rpar staticRootCost  . uncurry zip . ((^.  continuousBin) *** (^.  continuousBin))
    , parmap rpar staticRootCost  . uncurry zip . ((^. nonAdditiveBin) *** (^. nonAdditiveBin))
    , parmap rpar staticRootCost  . uncurry zip . ((^.    additiveBin) *** (^.    additiveBin))
    , parmap rpar staticRootCost  . uncurry zip . ((^.      metricBin) *** (^.      metricBin))
    , parmap rpar staticRootCost  . uncurry zip . ((^.   nonMetricBin) *** (^.   nonMetricBin))
    , parmap rpar dynamicRootCost . uncurry zip . ((^.     dynamicBin) *** (^.     dynamicBin))
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
staticCost :: HasBlockCost u v w x y z => MetadataBlock m -> CharacterBlock u v w x y z -> Double
staticCost mBlock cBlock = nestedSum $
    [ parmap rpar floatingCost . uncurry zip . ((^.  continuousBin) *** (^.  continuousBin))
    , parmap rpar integralCost . uncurry zip . ((^. nonAdditiveBin) *** (^. nonAdditiveBin))
    , parmap rpar integralCost . uncurry zip . ((^.    additiveBin) *** (^.    additiveBin))
    , parmap rpar integralCost . uncurry zip . ((^.      metricBin) *** (^.      metricBin))
    , parmap rpar integralCost . uncurry zip . ((^.   nonMetricBin) *** (^.   nonMetricBin))
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

hexFold :: Monoid m => CharacterBlock m m m m m m -> m
hexFold cBlock =
  fold
    (
    [ fold $ (^.  continuousBin) cBlock
    , fold $ (^. nonAdditiveBin) cBlock
    , fold $ (^.    additiveBin) cBlock
    , fold $ (^.      metricBin) cBlock
    , fold $ (^.   nonMetricBin) cBlock
    ] `using` evalList rpar
    )


-- |
-- Perform a nested sum traversing the list only once keeping
-- the accumulator in weak head normal form.
nestedSum
  :: (Foldable f1, Foldable f2, Num a)
  => f1 (f2 a) -> a
nestedSum = getSum . foldMap' (foldMap' Sum)
