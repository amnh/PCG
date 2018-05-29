------------------------------------------------------------------------------
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
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
--import           Bio.Sequence.Block.Internal
import           Bio.Sequence.Block.Character
import           Bio.Sequence.Block.Metadata
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Vector.Instances                ()
--import qualified Data.Vector                   as V


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z i r =
    ( HasCharacterCost   u r
    , HasCharacterCost   v i
    , HasCharacterCost   w i
    , HasCharacterCost   x i
    , HasCharacterCost   y i
    , HasCharacterCost   z i
    , HasCharacterWeight u r
    , HasCharacterWeight v r
    , HasCharacterWeight w r
    , HasCharacterWeight x r
    , HasCharacterWeight y r
    , HasCharacterWeight z r
    , Integral i
    , Real     r
    )


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasRootCost u v w x y z r =
    ( HasCharacterWeight   u r
    , HasCharacterWeight   v r
    , HasCharacterWeight   w r
    , HasCharacterWeight   x r
    , HasCharacterWeight   y r
    , HasCharacterWeight   z r
    , HasAverageLength     z AverageLength
    , PossiblyMissingCharacter u
    , PossiblyMissingCharacter v
    , PossiblyMissingCharacter w
    , PossiblyMissingCharacter x
    , PossiblyMissingCharacter y
    , PossiblyMissingCharacter z
    , Floating r
    , Real     r
    )


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
blockCost :: HasBlockCost u v w x y z i r => CharacterBlock u v w x y z -> r
blockCost block = sum . fmap sum $
    [ parmap rpar floatingCost . continuousCharacterBins 
    , parmap rpar integralCost . nonAdditiveCharacterBins
    , parmap rpar integralCost . additiveCharacterBins   
    , parmap rpar integralCost . metricCharacterBins     
    , parmap rpar integralCost . nonMetricCharacterBins  
    , parmap rpar integralCost . dynamicCharacters       
    ] <*> [block]
  where
    integralCost dec = fromIntegral cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight

    floatingCost dec = cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight


-- |
-- Calculate the "rooting cost" of a 'CharacterBlock' by applying a "rooting-
-- multiplier" based on the number of other roots in the DAG.
rootCost
  :: ( HasRootCost u v w x y z r
     , Integral i
     )
  => i
  -> CharacterBlock u v w x y z
  -> r
rootCost rootCount block = rootMultiplier . sum . fmap sum $
    [ parmap rpar staticRootCost  . continuousCharacterBins 
    , parmap rpar staticRootCost  . nonAdditiveCharacterBins
    , parmap rpar staticRootCost  . additiveCharacterBins
    , parmap rpar staticRootCost  . metricCharacterBins     
    , parmap rpar staticRootCost  . nonMetricCharacterBins  
    , parmap rpar dynamicRootCost . dynamicCharacters       
    ] <*> [block]
  where
    rootMultiplier x = (otherRoots * x) / 2
      where
        otherRoots = max 0 (fromIntegral rootCount - 1)
    
    staticRootCost dec
      | isMissing dec = 0
      | otherwise     = weight
      where
        weight = dec ^. characterWeight

    dynamicRootCost dec
      | isMissing dec = 0
      | otherwise     = weight * getAverageLength avgLen
      where
        avgLen = dec ^. averageLength
        weight = dec ^. characterWeight


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
staticCost :: HasBlockCost u v w x y z i r => CharacterBlock u v w x y z -> r
staticCost block = sum . fmap sum $
    [ parmap rpar floatingCost . continuousCharacterBins 
    , parmap rpar integralCost . nonAdditiveCharacterBins
    , parmap rpar integralCost . additiveCharacterBins   
    , parmap rpar integralCost . metricCharacterBins     
    , parmap rpar integralCost . nonMetricCharacterBins  
    ] <*> [block]
  where
    integralCost dec = fromIntegral cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight

    floatingCost dec = cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight

