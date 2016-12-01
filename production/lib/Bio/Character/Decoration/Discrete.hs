-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Discrete
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

-- For derived instance of PossiblyMissingCharacter
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Decoration.Discrete
  ( DiscreteDecoration()
  , DiscreteCharacterDecoration(..)
  , DiscreteCharacterMetadata()
  , GeneralCharacterMetadata()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..) 
  ) where


import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Control.Lens

import Bio.Metadata.CharacterName
import Data.Alphabet
import Data.TCM


data DiscreteDecoration c
   = DiscreteDec
   { additiveDecorationInitialCharacter :: c
   , metadata                           :: DiscreteCharacterMetadataDec c
   }


-- |
-- A 'Lens' for the 'discreteCharacter' field
class HasDiscreteCharacter s a | s -> a where

    discreteCharacter :: Lens' s a
    {-# MINIMAL discreteCharacter #-}


class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      , DiscreteCharacterMetadata s a
      ) => DiscreteCharacterDecoration s a | s -> a where 

    toDiscreteCharacterDecoration :: CharacterName -> Double -> Alphabet String -> TCM -> (x -> a) -> x -> s
    {-# MINIMAL toDiscreteCharacterDecoration #-}


instance ( DiscreteCharacterDecoration s a 
         , PossiblyMissingCharacter a
         ) => PossiblyMissingCharacter s where

    isMissing = isMissing . (^. discreteCharacter)

    toMissing x = x & discreteCharacter %~ toMissing



-- | (✔)
instance HasDiscreteCharacter (DiscreteDecoration c) c where

    discreteCharacter = lens additiveDecorationInitialCharacter (\e x -> e { additiveDecorationInitialCharacter = x })


-- | (✔)
instance HasCharacterAlphabet (DiscreteDecoration c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DiscreteDecoration c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DiscreteDecoration c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance EncodableStreamElement c => HasCharacterTransitionCostMatrix (DiscreteDecoration c) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }
        

-- | (✔)
instance HasCharacterWeight (DiscreteDecoration c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (DiscreteDecoration c) where

-- | (✔)
instance EncodableStreamElement c => DiscreteCharacterMetadata (DiscreteDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (DiscreteDecoration c) c where 
    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        DiscreteDec
        { additiveDecorationInitialCharacter = g symbolSet
        , metadata                           = discreteMetadata name weight alphabet tcm
        }    
