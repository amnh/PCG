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
  , DynamicCharacteracterMetadataDec()
  , DynamicCharacteracterMetadata(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
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
data DynamicCharacteracterMetadataDec c
   = DynamicCharacteracterMetadataDec
   { dataDenseTransitionCostMatrix :: !(Maybe DenseTransitionCostMatrix)
   , optimalTraversalFoci          :: !(Maybe TraversalFoci)
   , metadata                      :: {-# UNPACK #-} !(DiscreteWithTCMCharacterMetadataDec c)
   } deriving (Generic, NFData)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata s c
      , HasDenseTransitionCostMatrix     s (Maybe DenseTransitionCostMatrix)
--      , HasSparseTransitionCostMatrix    s  MemoizedCostMatrix
      , HasTraversalFoci                 s (Maybe TraversalFoci)
      ) => DynamicCharacteracterMetadata s c | s -> c where

    extractDynamicCharacteracterMetadata :: s -> DynamicCharacteracterMetadataDec c


instance Eq (DynamicCharacteracterMetadataDec c) where

    lhs == rhs = metadata lhs == metadata rhs && optimalTraversalFoci lhs == optimalTraversalFoci rhs


instance Show (DynamicCharacteracterMetadataDec c) where

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
instance DiscreteCharacterMetadata (DynamicCharacteracterMetadataDec c) where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . metadata


-- | (✔)
instance (Bits c, Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => DiscreteWithTcmCharacterMetadata (DynamicCharacteracterMetadataDec c) c where


-- | (✔)
instance (Bits c, Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => DynamicCharacteracterMetadata (DynamicCharacteracterMetadataDec c) c where

    {-# INLINE extractDynamicCharacteracterMetadata #-}
    extractDynamicCharacteracterMetadata = id


-- | (✔)
instance GeneralCharacterMetadata (DynamicCharacteracterMetadataDec c) where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . metadata


-- | (✔)
instance HasCharacterAlphabet (DynamicCharacteracterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens (\e -> metadata e ^. characterAlphabet)
                      $ \e x -> e { metadata = metadata e & characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicCharacteracterMetadataDec c) CharacterName where

    characterName = lens (\e -> metadata e ^. characterName)
                  $ \e x -> e { metadata = metadata e & characterName .~ x }


-- | (✔)
instance HasCharacterWeight (DynamicCharacteracterMetadataDec c) Double where

    characterWeight = lens (\e -> metadata e ^. characterWeight)
                    $ \e x -> e { metadata = metadata e & characterWeight .~ x }


-- | (✔)
instance HasDenseTransitionCostMatrix (DynamicCharacteracterMetadataDec c) (Maybe DenseTransitionCostMatrix) where

    denseTransitionCostMatrix = lens dataDenseTransitionCostMatrix $ \e x -> e { dataDenseTransitionCostMatrix = x }


{-
-- | (✔)
instance HasSparseTransitionCostMatrix (DynamicCharacteracterMetadataDec c) MemoizedCostMatrix where

    sparseTransitionCostMatrix = lens (\e -> metadata e ^. sparseTransitionCostMatrix)
                               $ \e x -> e { metadata = metadata e & sparseTransitionCostMatrix .~ x }
-}


-- | (✔)
instance HasSymbolChangeMatrix (DynamicCharacteracterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens (\e -> metadata e ^. symbolChangeMatrix)
                       $ \e x -> e { metadata = metadata e & symbolChangeMatrix .~ x }


-- | (✔)
instance (Bits c, Bound c ~ Word, Exportable c, Ranged c)
    => HasTransitionCostMatrix (DynamicCharacteracterMetadataDec c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens (\e -> metadata e ^. transitionCostMatrix)
                         $ \e x -> e { metadata = metadata e & transitionCostMatrix .~ x }


-- | (✔)
instance HasTraversalFoci (DynamicCharacteracterMetadataDec c) (Maybe TraversalFoci) where

    traversalFoci = lens optimalTraversalFoci $ \e x -> e { optimalTraversalFoci = x }


instance ToXML (DynamicCharacteracterMetadataDec c) where

    toXML input = xmlElement "Dynamic_Metadata" attrs contents
      where
        attrs    = []
        contents = [ Right . toXML $ metadata input
                   , Right . toXML $ fmap (fmap mutuallyExclusivePairs) <$> optimalTraversalFoci input
                   ]


-- |
-- Construct a concrete typed 'DynamicCharacteracterMetadataDec' value from the supplied inputs.
dynamicMetadata :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> Maybe DenseTransitionCostMatrix -> DynamicCharacteracterMetadataDec c
dynamicMetadata name weight alpha scm denseMay =
    force DynamicCharacteracterMetadataDec
    { dataDenseTransitionCostMatrix = denseMay
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name weight alpha scm
    }


-- |
-- Construct a concrete typed 'DynamicCharacteracterMetadataDec' value from the supplied inputs.
dynamicMetadataFromTCM :: CharacterName -> Double -> Alphabet String -> TCM -> DynamicCharacteracterMetadataDec c
dynamicMetadataFromTCM name weight alpha tcm =
    force DynamicCharacteracterMetadataDec
    { dataDenseTransitionCostMatrix = denseTCM
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name (coefficient * weight) alpha sigma
    }
  where
    sigma :: Word -> Word -> Word
    sigma i j   = fromIntegral $ factoredTcm diagnosis ! (fromEnum i, fromEnum j)
    coefficient = fromIntegral $ factoredWeight diagnosis
    diagnosis   = diagnoseTcm tcm
    denseTCM = maybeConstructDenseTransitionCostMatrix alpha sigma


-- |
-- Construct a concrete typed 'DynamicCharacteracterMetadataDec' value from the supplied inputs.
dynamicMetadataWithTCM :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> DynamicCharacteracterMetadataDec c
dynamicMetadataWithTCM name weight alpha scm =
    force DynamicCharacteracterMetadataDec
    { dataDenseTransitionCostMatrix = denseTCM
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name weight alpha scm
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
