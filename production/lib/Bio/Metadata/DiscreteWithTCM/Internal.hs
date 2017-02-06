
-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.DiscreteWithTCM.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Metadata.DiscreteWithTCM.Internal
  ( DiscreteWithTCMCharacterMetadataDec()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , discreteMetadataFromTCM
  , discreteMetadataWithTCM
  ) where


import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM.Class
import Bio.Metadata.General
import Control.Lens
import Data.Alphabet
import Data.List (intercalate)
import Data.Monoid
import Data.TCM


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets. 
data DiscreteWithTCMCharacterMetadataDec c
   = DiscreteWithTCMCharacterMetadataDec
   { symbolChangeMatrixData   :: Word -> Word -> Word
   , transitionCostMatrixData :: c -> c -> (c, Word)
   , discreteData             :: DiscreteCharacterMetadataDec
   }


instance Eq (DiscreteWithTCMCharacterMetadataDec c) where

    lhs == rhs = discreteData lhs == discreteData rhs
              && and [ symbolChangeMatrixData lhs i j == symbolChangeMatrixData rhs i j
                     | i <- range
                     , j <- range
                     ]
      where
        dimension = length $ lhs ^. characterAlphabet
        range     = toEnum <$> [ 0 .. dimension - 1 ]


instance Show (DiscreteWithTCMCharacterMetadataDec c) where

    show e = intercalate "\n"
        [ "DiscreteCharacterMetadata"
        , "  CharacterName: " <> show (e ^. characterName    )
        , "  Alphabet:      " <> show (e ^. characterAlphabet)
        , "  Weight:        " <> show (e ^. characterWeight  )
        , "  TCM: "
        , show . generate dimension $ \(i,j) -> cost (toEnum i) (toEnum j)
        ]
      where
        cost      = e ^. symbolChangeMatrix
        dimension = length $ e ^. characterAlphabet


-- | (✔) 
instance GeneralCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata =  extractGeneralCharacterMetadata . discreteData
      
  
-- | (✔)
instance DiscreteCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = discreteData


-- | (✔)
instance HasCharacterAlphabet (DiscreteWithTCMCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens (\e -> discreteData e ^. characterAlphabet)
                      $ \e x -> e { discreteData = discreteData e & characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DiscreteWithTCMCharacterMetadataDec c) CharacterName where

    characterName = lens (\e -> discreteData e ^. characterName)
                  $ \e x -> e { discreteData = discreteData e & characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasSymbolChangeMatrix (DiscreteWithTCMCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens symbolChangeMatrixData $ \e x -> e { symbolChangeMatrixData = x }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasTransitionCostMatrix (DiscreteWithTCMCharacterMetadataDec c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens undefined undefined


-- | (✔)
instance HasCharacterWeight (DiscreteWithTCMCharacterMetadataDec c) Double where

    characterWeight = lens (\e -> discreteData e ^. characterWeight)
                    $ \e x -> e { discreteData = discreteData e & characterWeight .~ x }


-- |
-- Construct a concrete typed 'DiscreteWithTCMCharacterMetadataDec' value from the supplied inputs.
discreteMetadataFromTCM :: CharacterName -> Double -> Alphabet String -> TCM -> DiscreteWithTCMCharacterMetadataDec c
discreteMetadataFromTCM name weight alpha tcm =
    DiscreteWithTCMCharacterMetadataDec
    { symbolChangeMatrixData   = \i j -> toEnum . fromEnum $ factoredTcm diagnosis ! (fromEnum i, fromEnum j)
    , transitionCostMatrixData = undefined
    , discreteData             = discreteMetadata name (weight * coefficient) alpha
    }
  where
    diagnosis   = diagnoseTcm tcm
    coefficient = fromIntegral $ factoredWeight diagnosis
           


-- |
-- Construct a concrete typed 'DiscreteWithTCMCharacterMetadataDec' value from the supplied inputs.
discreteMetadataWithTCM :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> DiscreteWithTCMCharacterMetadataDec c
discreteMetadataWithTCM name weight alpha scm =
    DiscreteWithTCMCharacterMetadataDec
    { symbolChangeMatrixData   = scm
    , transitionCostMatrixData = undefined
    , discreteData             = discreteMetadata name weight alpha
    }
           
