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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
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

import Bio.Metadata.Discrete.Class
import Bio.Metadata.General
import Bio.Metadata.Metric
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.Binary
import Data.Bits
import Data.CharacterName
import Data.FileSource
import Data.List                   (intercalate)
import Data.MetricRepresentation
import GHC.Generics                hiding (to)
import Text.XML


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continuous bins do not have Alphabets.
data  DiscreteCharacterMetadataDec
    = DiscreteCharacterMetadataDec
    { alphabet      :: {-# UNPACK #-} !(Alphabet String)
    , generalData   :: {-# UNPACK #-} !GeneralCharacterMetadataDec
    , tcmSourceFile :: {-# UNPACK #-} !FileSource
    }
    deriving stock    (Generic)
    deriving anyclass (Binary, NFData)


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


instance Show DiscreteCharacterMetadataDec where

    show e = intercalate "\n"
      [ "DiscreteCharacterMetadata"
      , "  CharacterName: " <> show (e ^. characterName    )
      , "  Alphabet:      " <> show (e ^. characterAlphabet)
      , "  Weight:        " <> show (e ^. characterWeight  )
      , "  TCMSourceFile: " <> show (e ^. _tcmSourceFile   )
      ]


instance DiscreteCharacterMetadata DiscreteCharacterMetadataDec where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = id


instance GeneralCharacterMetadata DiscreteCharacterMetadataDec where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata = generalData

instance (Bits c, Num b) =>
  GetPairwiseTransitionCostMatrix DiscreteCharacterMetadataDec c b where

    pairwiseTransitionCostMatrix = to $ const discreteMetricPairwiseLogic


instance HasCharacterAlphabet DiscreteCharacterMetadataDec (Alphabet String) where

    characterAlphabet = lens alphabet $ \e x -> e { alphabet = x }


instance HasCharacterName DiscreteCharacterMetadataDec CharacterName where

    characterName = lens (\e -> generalData e ^. characterName)
                  $ \e x -> e { generalData = generalData e & characterName .~ x }


instance HasCharacterWeight DiscreteCharacterMetadataDec Double where

    characterWeight = lens (\e -> generalData e ^. characterWeight)
                    $ \e x -> e { generalData = generalData e & characterWeight .~ x }

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
