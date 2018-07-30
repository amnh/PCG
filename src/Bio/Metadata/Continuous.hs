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

{-# LANGUAGE DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Metadata.Continuous
  ( ContinuousCharacterMetadataDec()
  , GeneralCharacterMetadata(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..) 
  , continuousMetadata
  ) where


import Bio.Metadata.CharacterName
import Bio.Metadata.General
import Control.DeepSeq
import Control.Lens
import GHC.Generics
import Text.XML


-- |
-- Metadata type for a continuous character.
newtype ContinuousCharacterMetadataDec = CCM GeneralCharacterMetadataDec
  deriving (Generic)


-- | (✔)
instance GeneralCharacterMetadata ContinuousCharacterMetadataDec where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata (CCM x) = x


-- | (✔)
instance HasCharacterName ContinuousCharacterMetadataDec CharacterName where

    characterName = lens (\(CCM e) -> e ^. characterName) $ \(CCM e) x -> CCM (e & characterName .~ x)


-- | (✔)
instance HasCharacterWeight ContinuousCharacterMetadataDec Double where

    characterWeight = lens (\(CCM e) -> e ^. characterWeight) $ \(CCM e) x -> CCM (e & characterWeight .~ x)


instance NFData ContinuousCharacterMetadataDec


instance ToXML ContinuousCharacterMetadataDec where

    toXML (CCM gm) = toXML gm

-- |
-- A smart constructor for 'GeneralCharacterMetadata'.
continuousMetadata :: CharacterName -> Double -> ContinuousCharacterMetadataDec
continuousMetadata name weight = CCM $ generalMetadata name weight

