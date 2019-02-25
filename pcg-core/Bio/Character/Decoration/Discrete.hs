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

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- For derived instance of PossiblyMissingCharacter
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Decoration.Discrete
  ( DiscreteDecoration()
  , DiscreteCharacterDecoration()
  , DiscreteCharacterMetadata()
  , GeneralCharacterMetadata()
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , PossiblyMissingCharacter(..)
  , SimpleDiscreteCharacterDecoration(..)
  ) where


import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens
import Data.Range
import Data.String
import Numeric.Extended
import Text.XML
import TextShow                        (TextShow (showb))


-- |
-- General, concrete type for 'Discrete' characters.
newtype DiscreteDecoration c = DiscreteDec { discreteDecorationCharacter :: c }


-- |
-- A 'Lens' for the 'discreteCharacter' field
class HasDiscreteCharacter s a | s -> a where

    discreteCharacter :: Lens' s a
    {-# MINIMAL discreteCharacter #-}


-- | (✔)
class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      ) => DiscreteCharacterDecoration s a | s -> a where


-- | (✔)
class DiscreteCharacterDecoration s a => SimpleDiscreteCharacterDecoration s a | s -> a where

    toDiscreteCharacterDecoration :: (x -> a) -> x -> s
    {-# MINIMAL toDiscreteCharacterDecoration #-}



instance Show c => Show (DiscreteDecoration c) where

    show = show . (^. discreteCharacter)


instance TextShow c => TextShow (DiscreteDecoration c) where

    showb = showb . (^. discreteCharacter)


-- | (✔)
instance HasDiscreteCharacter (DiscreteDecoration c) c where

    discreteCharacter = lens discreteDecorationCharacter (\e x -> e { discreteDecorationCharacter = x })


-- | (✔)
instance HasIntervalCharacter (DiscreteDecoration c) c where

    intervalCharacter = discreteCharacter


-- | (✔)
instance (Ranged c, ExtendedNumber (Bound c), Num (Finite (Bound c)), Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (DiscreteDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (DiscreteDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (DiscreteDecoration c) c where

    toDiscreteCharacterDecoration g symbolSet =
        DiscreteDec
        { discreteDecorationCharacter = g symbolSet
        }


-- | (✔)
instance (Show c) => ToXML (DiscreteDecoration c) where

    toXML decoration = xmlElement "Discrete_character_decoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Character", show $ decoration ^. discreteCharacter)
                         , Left ("Metadata" , "TCM not shown"                       )
                         ]


-- | (✔)
instance {-# OVERLAPPABLE #-} (HasDiscreteCharacter s c, PossiblyMissingCharacter c) => PossiblyMissingCharacter s where

    isMissing = isMissing . (^. discreteCharacter)

    toMissing x = x & discreteCharacter %~ toMissing
