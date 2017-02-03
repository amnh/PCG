-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Bio.Metadata.Dynamic.Internal
  ( DynamicCharacterMetadataDec()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , dynamicMetadata
  ) where


import Analysis.Parsimony.Dynamic.DirectOptimization.FFI
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic.Class
import Bio.Metadata.General
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.List (intercalate)
import Data.Monoid
import Data.TCM
import GHC.Generics (Generic)

import Debug.Trace


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets. 
data DynamicCharacterMetadataDec c
   = DynamicCharacterMetadataDec
   { dataCharacterAlphabet         :: Alphabet String
   , dataCharacterName             :: CharacterName
   , dataCharacterWeight           :: Double
   , dataSymbolChangeMatrix        :: Word -> Word -> Word
   , dataTransitionCostMatrix      :: c -> c -> (c, Word)
   , dataDenseTransitionCostMatrix :: Maybe DenseTransitionCostMatrix
   } deriving (Generic)


instance NFData (DynamicCharacterMetadataDec a) where

  rnf (DynamicCharacterMetadataDec a n w _ _ d) = ()
    where
      !_ = rnf a
      !_ = rnf n
      !_ = rnf w
      !_ = rnf d


instance Eq (DynamicCharacterMetadataDec c) where

    lhs == rhs = dataCharacterAlphabet lhs == dataCharacterAlphabet rhs
              && dataCharacterName     lhs == dataCharacterName     rhs
              && dataCharacterWeight   lhs == dataCharacterWeight   rhs
              && and [ dataSymbolChangeMatrix lhs i j == dataSymbolChangeMatrix rhs i j
                     | i <- range
                     , j <- range
                     ]
      where
        dimension = length $ dataCharacterAlphabet lhs
        range     = toEnum <$> [0 .. dimension - 1 ]


instance Show (DynamicCharacterMetadataDec c) where

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


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
instance EncodableStreamElement c => DiscreteCharacterMetadata (DynamicCharacterMetadataDec c) where


-- | (✔) 
instance GeneralCharacterMetadata (DynamicCharacterMetadataDec c) where
  
  
-- | (✔)
instance HasCharacterAlphabet (DynamicCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens dataCharacterAlphabet $ \e x -> e { dataCharacterAlphabet = x }


-- | (✔)
instance HasCharacterName (DynamicCharacterMetadataDec c) CharacterName where

    characterName = lens dataCharacterName
                  $ \e x -> e { dataCharacterName = x }


-- | (✔)
instance HasCharacterWeight (DynamicCharacterMetadataDec c) Double where

    characterWeight = lens dataCharacterWeight $ \e x -> e { dataCharacterWeight = x }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasDenseTransitionCostMatrix (DynamicCharacterMetadataDec c) (Maybe DenseTransitionCostMatrix) where

    denseTransitionCostMatrix = lens dataDenseTransitionCostMatrix $ \e x -> e { dataDenseTransitionCostMatrix = x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasSymbolChangeMatrix (DynamicCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens dataSymbolChangeMatrix $ \e x -> e { dataSymbolChangeMatrix = x }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasTransitionCostMatrix (DynamicCharacterMetadataDec c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens undefined undefined


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadata :: CharacterName -> Double -> Alphabet String -> TCM -> DynamicCharacterMetadataDec c
dynamicMetadata name weight alpha tcm =
    force DynamicCharacterMetadataDec
    { dataCharacterAlphabet         = alpha
    , dataCharacterName             = name
    , dataCharacterWeight           = coefficient * weight
    , dataSymbolChangeMatrix        = sigma
    , dataTransitionCostMatrix      = undefined
    , dataDenseTransitionCostMatrix = denseTCM
    }
  where
    sigma :: Word -> Word -> Word
    sigma i j   = fromIntegral $ factoredTcm diagnosis ! (fromEnum i, fromEnum j)
    coefficient = fromIntegral $ factoredWeight diagnosis
    diagnosis   = diagnoseTcm tcm
    denseTCM
      | len > 8   = Nothing
      | otherwise = Just $ generateDenseTransitionCostMatrix len sigma
      where
        len = toEnum $ length alpha
