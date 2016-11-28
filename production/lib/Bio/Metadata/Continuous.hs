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

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Metadata.Continuous
  ( GeneralCharacterMetadata()
  , HasCharacterName(..)
  , HasCharacterWeight(..) 
  , continuousMetadata
  ) where


import Bio.Metadata.CharacterName
import Bio.Metadata.General
import Control.Lens


newtype ContinuousCharacterMetadataDec = CCM GeneralCharacterMetadataDec


-- | (✔)
instance GeneralCharacterMetadata ContinuousCharacterMetadataDec where


-- | (✔)
instance HasCharacterName ContinuousCharacterMetadataDec CharacterName where

    characterName = lens (\(CCM e) -> e ^. characterName) $ \(CCM e) x -> CCM (e & characterName .~ x)


-- | (✔)
instance HasCharacterWeight ContinuousCharacterMetadataDec Double where

    characterWeight = lens (\(CCM e) -> e ^. characterWeight) $ \(CCM e) x -> CCM (e & characterWeight .~ x)


-- |
-- A smart constructor for 'GeneralCharacterMetadata'.
continuousMetadata :: CharacterName -> Double -> ContinuousCharacterMetadataDec
continuousMetadata name weight = CCM $ generalMetadata name weight

