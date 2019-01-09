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

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Bio.Metadata.Dynamic.Internal
  ( DenseTransitionCostMatrix
  , DynamicCharacterMetadataDec()
  , DynamicCharacterMetadata(..)
  , GetSymbolChangeMatrix(..)
  , GetTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , MemoizedCostMatrix()
  , TraversalFoci
  , TraversalFocusEdge
  , TraversalTopology
  , dynamicMetadata
  , dynamicMetadataFromTCM
  , dynamicMetadataWithTCM
  , maybeConstructDenseTransitionCostMatrix
  ) where


import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise (generateDenseTransitionCostMatrix)
import Bio.Character.Encodable
import Bio.Character.Exportable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic.Class
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.Bits
import Data.List                                              (intercalate)
import Data.List.NonEmpty                                     (NonEmpty)
import Data.Range
import Data.TCM
import Data.Text.Short                                        (ShortText)
import Data.TopologyRepresentation
import GHC.Generics                                           (Generic)
import Text.XML


-- |
-- A unique representation of a DAG topology.
type TraversalTopology  = TopologyRepresentation TraversalFocusEdge


-- |
-- An unrooted edge.
type TraversalFocusEdge = (Int, Int)


type TraversalFocus     = (TraversalFocusEdge, TraversalTopology)


-- |
-- Represents a collection of paired rooting edges and unrooted topologies.
type TraversalFoci      = NonEmpty TraversalFocus


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets.
data DynamicCharacterMetadataDec c
   = DynamicCharacterMetadataDec
   { dataDenseTransitionCostMatrix :: !(Maybe DenseTransitionCostMatrix)
   , optimalTraversalFoci          :: !(Maybe TraversalFoci)
   , metadata                      :: {-# UNPACK #-} !(DiscreteWithTCMCharacterMetadataDec c)
   } deriving (Generic, NFData)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata s c
      , GetDenseTransitionCostMatrix     s (Maybe DenseTransitionCostMatrix)
      , GetSparseTransitionCostMatrix    s (Maybe MemoizedCostMatrix)
      , HasTraversalFoci                 s (Maybe TraversalFoci)
      ) => DynamicCharacterMetadata s c | s -> c where

    extractDynamicCharacterMetadata :: s -> DynamicCharacterMetadataDec c


instance Eq (DynamicCharacterMetadataDec c) where

    lhs == rhs = metadata lhs == metadata rhs && optimalTraversalFoci lhs == optimalTraversalFoci rhs


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


-- | (✔)
instance DiscreteCharacterMetadata (DynamicCharacterMetadataDec c) where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . metadata


-- | (✔)
instance (Bits c, Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => DiscreteWithTcmCharacterMetadata (DynamicCharacterMetadataDec c) c where


-- | (✔)
instance (Bits c, Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => DynamicCharacterMetadata (DynamicCharacterMetadataDec c) c where

    {-# INLINE extractDynamicCharacterMetadata #-}
    extractDynamicCharacterMetadata = id


-- | (✔)
instance GeneralCharacterMetadata (DynamicCharacterMetadataDec c) where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . metadata


-- | (✔)
instance HasCharacterAlphabet (DynamicCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens (\e -> metadata e ^. characterAlphabet)
                      $ \e x -> e { metadata = metadata e & characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicCharacterMetadataDec c) CharacterName where

    characterName = lens (\e -> metadata e ^. characterName)
                  $ \e x -> e { metadata = metadata e & characterName .~ x }


-- | (✔)
instance HasCharacterWeight (DynamicCharacterMetadataDec c) Double where

    characterWeight = lens (\e -> metadata e ^. characterWeight)
                    $ \e x -> e { metadata = metadata e & characterWeight .~ x }


-- | (✔)
instance GetDenseTransitionCostMatrix (DynamicCharacterMetadataDec c) (Maybe DenseTransitionCostMatrix) where

    denseTransitionCostMatrix = to dataDenseTransitionCostMatrix

-- | (✔)
instance HasTcmSourceFile (DynamicCharacterMetadataDec c) ShortText where

    _tcmSourceFile = lens (\d -> metadata d ^. _tcmSourceFile)
                   $ \d s -> d { metadata = metadata d & _tcmSourceFile .~ s }

-- | (✔)
instance GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec c) (Maybe MemoizedCostMatrix) where

    sparseTransitionCostMatrix = to (\e -> metadata e ^. sparseTransitionCostMatrix)


-- | (✔)
instance GetSymbolChangeMatrix (DynamicCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = to (\e -> metadata e ^. symbolChangeMatrix)


-- | (✔)
instance (Bits c, Bound c ~ Word, Exportable c, Ranged c)
    => GetTransitionCostMatrix (DynamicCharacterMetadataDec c) (c -> c -> (c, Word)) where

    transitionCostMatrix = to (\e -> metadata e ^. transitionCostMatrix)


-- | (✔)
instance HasTraversalFoci (DynamicCharacterMetadataDec c) (Maybe TraversalFoci) where

    traversalFoci = lens optimalTraversalFoci $ \e x -> e { optimalTraversalFoci = x }


instance ToXML (DynamicCharacterMetadataDec c) where

    toXML input = xmlElement "Dynamic_Metadata" attrs contents
      where
        attrs    = []
        contents = [ Right . toXML $ metadata input
                   , Right . toXML $ fmap (fmap mutuallyExclusivePairs) <$> optimalTraversalFoci input
                   ]


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadata
  :: CharacterName
  -> Double
  -> Alphabet String
  -> ShortText
  -> (Word -> Word -> Word)
  -> Maybe DenseTransitionCostMatrix
  -> DynamicCharacterMetadataDec c
dynamicMetadata name weight alpha tcmSource scm denseMay =
    force DynamicCharacterMetadataDec
    { dataDenseTransitionCostMatrix = denseMay
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name weight alpha tcmSource scm
    }


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataFromTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> ShortText -> TCM -> DynamicCharacterMetadataDec c
dynamicMetadataFromTCM name weight alpha tcmSource tcm =
    force DynamicCharacterMetadataDec
    { dataDenseTransitionCostMatrix = denseTCM
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name (coefficient * weight) alpha tcmSource sigma
    }
  where
    sigma :: Word -> Word -> Word
    sigma i j   = fromIntegral $ factoredTcm diagnosis ! (fromEnum i, fromEnum j)
    coefficient = fromIntegral $ factoredWeight diagnosis
    diagnosis   = diagnoseTcm tcm
    denseTCM = maybeConstructDenseTransitionCostMatrix alpha sigma


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataWithTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> ShortText
  -> (Word -> Word -> Word)
  -> DynamicCharacterMetadataDec c
dynamicMetadataWithTCM name weight alpha tcmSource scm =
    force DynamicCharacterMetadataDec
    { dataDenseTransitionCostMatrix = denseTCM
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name weight alpha tcmSource scm
    }
  where
    denseTCM = maybeConstructDenseTransitionCostMatrix alpha scm


-- |
-- /O(n^3)/ Constructs 2D & 3D dense TCMs.
--
-- Conditionally construct a 'DenseTransitionCostMatrix'. If the alphabet size is
-- too large, a @Nothing@ value will be returned. Otherwise the dense TCM is
-- constructed strictly at the time the function in invoked.
--
-- Currentlty returns a @Just@ value for alphabet sizes in the range @[2..8]@.
maybeConstructDenseTransitionCostMatrix :: Alphabet a -> (Word -> Word -> Word) -> Maybe DenseTransitionCostMatrix
maybeConstructDenseTransitionCostMatrix alpha sigma = force f
  where
    f
      | len > 8   = Nothing
      | otherwise = Just $ generateDenseTransitionCostMatrix 0 len sigma
      where
        len = toEnum $ length alpha
