------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Bio.Metadata.Continuous
  ( ContinuousCharacterMetadataDec()
  , GeneralCharacterMetadata(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , continuousMetadata
  ) where

import Bio.Metadata.General
import Bio.Metadata.Metric
import Control.DeepSeq
import Control.Lens
import Data.Binary
import Data.CharacterName
import Data.MetricRepresentation
import Data.Range
import GHC.Generics               hiding (to)
import Text.XML


-- |
-- Metadata type for a continuous character.
newtype ContinuousCharacterMetadataDec = CCM GeneralCharacterMetadataDec
  deriving stock   (Generic, Show)
  deriving newtype (Binary, NFData)


instance GeneralCharacterMetadata ContinuousCharacterMetadataDec where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata (CCM x) = x

instance forall c d . (Ranged c, Bound c ~ d, Ord d) =>
  GetPairwiseTransitionCostMatrix ContinuousCharacterMetadataDec c d where

    pairwiseTransitionCostMatrix = to $ const (firstLinearNormPairwiseLogic @c @c @c)


instance HasCharacterName ContinuousCharacterMetadataDec CharacterName where

    characterName = lens (\(CCM e) -> e ^. characterName) $ \(CCM e) x -> CCM (e & characterName .~ x)


instance HasCharacterWeight ContinuousCharacterMetadataDec Double where

    characterWeight = lens (\(CCM e) -> e ^. characterWeight) $ \(CCM e) x -> CCM (e & characterWeight .~ x)


instance ToXML ContinuousCharacterMetadataDec where

    toXML (CCM gm) = toXML gm

-- |
-- A smart constructor for 'GeneralCharacterMetadata'.
continuousMetadata :: CharacterName -> Double -> ContinuousCharacterMetadataDec
continuousMetadata name weight = CCM $ generalMetadata name weight

