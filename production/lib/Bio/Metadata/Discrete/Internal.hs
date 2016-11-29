-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Discrete.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Metadata.Discrete.Internal
  ( DiscreteCharacterMetadataDec()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , discreteMetadata
  ) where


import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete.Class
import Bio.Metadata.General
import Control.Lens
import Data.Alphabet
import Data.List (intercalate)
import Data.Monoid
import Data.TCM


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets. 
data DiscreteCharacterMetadataDec c
   = DiscreteCharacterMetadataDec
   { alphabet       :: Alphabet String
   , unambiguousTCM :: TCM
   , fullTCM        :: (c -> c -> (c,Int))
   , generalData    :: GeneralCharacterMetadataDec
   }


instance Eq (DiscreteCharacterMetadataDec c) where

    lhs == rhs = and
      [ alphabet       lhs == alphabet       rhs
      , unambiguousTCM lhs == unambiguousTCM rhs
      , generalData    lhs == generalData    rhs
      ]

instance Show (DiscreteCharacterMetadataDec c) where

    show e = intercalate "\n"
      [ "DiscreteCharacterMetadata"
      , "  CharacterName: " <> show (e ^. characterName    )
      , "  Alphabet:      " <> show (e ^. characterAlphabet)
      , "  Weight:        " <> show (e ^. characterWeight  )
      , "  TCM: "
      , show $ unambiguousTCM e
      ]



-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
instance EncodableStreamElement c => DiscreteCharacterMetadata (DiscreteCharacterMetadataDec c) c where


  -- | (✔) 
instance GeneralCharacterMetadata (DiscreteCharacterMetadataDec c) where
  
  
-- | (✔)
instance HasCharacterAlphabet (DiscreteCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens alphabet $ \e x -> e { alphabet = x }


-- | (✔)
instance HasCharacterName (DiscreteCharacterMetadataDec c) CharacterName where

    characterName = lens (\e -> generalData e ^. characterName)
                  $ \e x -> e { generalData = generalData e & characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DiscreteCharacterMetadataDec c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
        getter e   = (\i j -> fromIntegral $ unambiguousTCM e ! (i,j))
        setter e f = e { unambiguousTCM = newTCM }
          where
            dimension       = size $ unambiguousTCM e
            generator (i,j) = f i j
            newTCM          = generate dimension generator


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasCharacterTransitionCostMatrix (DiscreteCharacterMetadataDec c) (c -> c -> (c, Int)) where

    characterTCM = lens undefined undefined


-- | (✔)
instance HasCharacterWeight (DiscreteCharacterMetadataDec c) Double where

    characterWeight = lens (\e -> generalData e ^. characterWeight)
                    $ \e x -> e { generalData = generalData e & characterWeight .~ x }



discreteMetadata :: CharacterName -> Double -> Alphabet String -> TCM -> DiscreteCharacterMetadataDec c
discreteMetadata name weight alpha tcm =
    DiscreteCharacterMetadataDec
    { alphabet       = alpha
    , unambiguousTCM = tcm
    , fullTCM        = undefined
    , generalData    = generalMetadata name weight
    }
           
