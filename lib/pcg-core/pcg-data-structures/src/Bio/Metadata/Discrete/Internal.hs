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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Metadata.Discrete.Internal
  ( DiscreteCharacterMetadataDec()
  , DiscreteCharacterMetadata(..)
  , GeneralCharacterMetadata(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasTcmSourceFile(..)
  , discreteMetadata
  ) where


import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete.Class
import Bio.Metadata.General
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.FileSource
import Data.List                   (intercalate)
import GHC.Generics
import Text.XML


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets.
data DiscreteCharacterMetadataDec
   = DiscreteCharacterMetadataDec
   { alphabet      :: {-# UNPACK #-} !(Alphabet String)
   , generalData   :: {-# UNPACK #-} !GeneralCharacterMetadataDec
   , tcmSourceFile :: {-# UNPACK #-} !FileSource
   } deriving (Generic)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( GeneralCharacterMetadata s
      , HasCharacterAlphabet     s (Alphabet String)
      ) => DiscreteCharacterMetadata s where

    extractDiscreteCharacterMetadata :: s -> DiscreteCharacterMetadataDec


instance Eq DiscreteCharacterMetadataDec where

    lhs == rhs = alphabet    lhs == alphabet       rhs
              && generalData lhs == generalData    rhs


instance NFData DiscreteCharacterMetadataDec


instance Show DiscreteCharacterMetadataDec where

    show e = intercalate "\n"
      [ "DiscreteCharacterMetadata"
      , "  CharacterName: " <> show (e ^. characterName    )
      , "  Alphabet:      " <> show (e ^. characterAlphabet)
      , "  Weight:        " <> show (e ^. characterWeight  )
      , "  TCMSourceFile: " <> show (e ^. _tcmSourceFile   )
      ]


-- | (✔)
instance DiscreteCharacterMetadata DiscreteCharacterMetadataDec where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = id


-- | (✔)
instance GeneralCharacterMetadata DiscreteCharacterMetadataDec where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata = generalData


-- | (✔)
instance HasCharacterAlphabet DiscreteCharacterMetadataDec (Alphabet String) where

    characterAlphabet = lens alphabet $ \e x -> e { alphabet = x }


-- | (✔)
instance HasCharacterName DiscreteCharacterMetadataDec CharacterName where

    characterName = lens (\e -> generalData e ^. characterName)
                  $ \e x -> e { generalData = generalData e & characterName .~ x }


-- | (✔)
instance HasCharacterWeight DiscreteCharacterMetadataDec Double where

    characterWeight = lens (\e -> generalData e ^. characterWeight)
                    $ \e x -> e { generalData = generalData e & characterWeight .~ x }

-- | (✔)
instance HasTcmSourceFile DiscreteCharacterMetadataDec FileSource where

    _tcmSourceFile = lens tcmSourceFile $ \d s -> d { tcmSourceFile = s }


instance ToXML DiscreteCharacterMetadataDec where

    toXML xmlElem = xmlElement "Discrete_metadata" attrs contents
        where
            attrs    = []
            contents = [ Right . toXML $ generalData xmlElem
                       , Left ("Alphabet", show $ xmlElem ^. characterAlphabet)
                       ]


-- |
-- Construct a concrete typed 'DiscreteCharacterMetadataDec' value from the supplied inputs.
discreteMetadata :: CharacterName -> Double -> Alphabet String -> FileSource -> DiscreteCharacterMetadataDec
discreteMetadata name weight alpha tcmSource =
    DiscreteCharacterMetadataDec
    { alphabet       = alpha
    , generalData    = generalMetadata name weight
    , tcmSourceFile  = tcmSource
    }
