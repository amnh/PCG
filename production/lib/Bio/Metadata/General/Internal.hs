------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.General.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Metadata.General.Internal
  ( GeneralCharacterMetadataDec()
  , GeneralCharacterMetadata(..)
  , generalMetadata
  ) where


import Bio.Metadata.CharacterName
import Bio.Metadata.General.Class
import Control.Lens
import Text.XML.Custom


-- |
-- Represents a concrete type containing metadata fields shared across different
-- bins.
data GeneralCharacterMetadataDec
   = GeneralCharacterMetadataDec
   { name     :: CharacterName
   , weight   :: Double
   } deriving (Eq, Show)


-- |
-- A constraint for type containing metadata.
class ( HasCharacterName   s CharacterName
      , HasCharacterWeight s Double
      ) => GeneralCharacterMetadata s where

    extractGeneralCharacterMetadata :: s -> GeneralCharacterMetadataDec


-- | (✔)
instance GeneralCharacterMetadata GeneralCharacterMetadataDec where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata = id


-- | (✔)
instance HasCharacterName GeneralCharacterMetadataDec CharacterName where

    characterName = lens name $ \e x -> e { name = x }


-- | (✔)
instance HasCharacterWeight GeneralCharacterMetadataDec Double where

    characterWeight = lens weight $ \e x -> e { weight = x }


instance ToXML GeneralCharacterMetadataDec where

    toXML metadata = xmlElement "Metadata" attributes contents
        where
            attributes = []
            contents   = [ ("Name"  , Left . show $ metadata ^. characterName  )
                         , ("Weight", Left . show $ metadata ^. characterWeight)
                         ]

-- |
-- A smart constructor for 'GeneralCharacterMetadata'.
generalMetadata :: CharacterName -> Double -> GeneralCharacterMetadataDec
generalMetadata = GeneralCharacterMetadataDec
