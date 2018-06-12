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
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Bio.Metadata.Dynamic.Internal
  ( DenseTransitionCostMatrix
  , DynamicCharacterMetadataDec()
  , DynamicCharacterMetadata(..)
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
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic.Class
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.List          (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.TCM
import Data.TopologyRepresentation
import GHC.Generics       (Generic)


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
   { dataDenseTransitionCostMatrix :: Maybe DenseTransitionCostMatrix
   , optimalTraversalFoci          :: Maybe TraversalFoci
   , metadata                      :: DiscreteWithTCMCharacterMetadataDec c
   } deriving (Generic)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata s c
      , HasDenseTransitionCostMatrix     s (Maybe DenseTransitionCostMatrix)
      , HasSparseTransitionCostMatrix    s  MemoizedCostMatrix
      , HasTraversalFoci                 s (Maybe TraversalFoci)
      ) => DynamicCharacterMetadata s c | s -> c where

    extractDynamicCharacterMetadata :: s -> DynamicCharacterMetadataDec c


instance Eq (DynamicCharacterMetadataDec c) where

    lhs == rhs = lhs ^. characterAlphabet == rhs ^. characterAlphabet
              && lhs ^. characterName     == rhs ^. characterName
              && lhs ^. characterWeight   == rhs ^. characterWeight
              && and [ (lhs ^. symbolChangeMatrix) i j == (rhs ^. symbolChangeMatrix) i j
                     | i <- range
                     , j <- range
                     ]
      where
        dimension = length $ lhs ^. characterAlphabet
        range     = toEnum <$> [0 .. dimension - 1 ]


instance NFData (DynamicCharacterMetadataDec a) where

    rnf (DynamicCharacterMetadataDec d e _) = ()
      where
        !_ = rnf d
        !_ = rnf e


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
instance EncodableStreamElement c => DiscreteWithTcmCharacterMetadata (DynamicCharacterMetadataDec c) c where


-- | (✔)
instance EncodableStreamElement c => DynamicCharacterMetadata (DynamicCharacterMetadataDec c) c where

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
instance HasDenseTransitionCostMatrix (DynamicCharacterMetadataDec c) (Maybe DenseTransitionCostMatrix) where

    denseTransitionCostMatrix = lens dataDenseTransitionCostMatrix $ \e x -> e { dataDenseTransitionCostMatrix = x }


-- | (✔)
instance HasSparseTransitionCostMatrix (DynamicCharacterMetadataDec c) MemoizedCostMatrix where

    sparseTransitionCostMatrix = lens (\e -> metadata e ^. sparseTransitionCostMatrix)
                               $ \e x -> e { metadata = metadata e & sparseTransitionCostMatrix .~ x }


-- | (✔)
instance HasSymbolChangeMatrix (DynamicCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens (\e -> metadata e ^. symbolChangeMatrix)
                       $ \e x -> e { metadata = metadata e & symbolChangeMatrix .~ x }


-- | (✔)
instance HasTransitionCostMatrix (DynamicCharacterMetadataDec c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens (\e -> metadata e ^. transitionCostMatrix)
                         $ \e x -> e { metadata = metadata e & transitionCostMatrix .~ x }


-- | (✔)
instance HasTraversalFoci (DynamicCharacterMetadataDec c) (Maybe TraversalFoci) where

    traversalFoci = lens optimalTraversalFoci $ \e x -> e { optimalTraversalFoci = x }


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadata :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> Maybe DenseTransitionCostMatrix -> DynamicCharacterMetadataDec c
dynamicMetadata name weight alpha scm denseMay =
    force DynamicCharacterMetadataDec
    { dataDenseTransitionCostMatrix = denseMay
    , optimalTraversalFoci          = Nothing
    , metadata                      = discreteMetadataWithTCM name weight alpha scm
    }


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataFromTCM :: CharacterName -> Double -> Alphabet String -> TCM -> DynamicCharacterMetadataDec c
dynamicMetadataFromTCM name weight alpha tcm =
    force DynamicCharacterMetadataDec
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
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataWithTCM :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> DynamicCharacterMetadataDec c
dynamicMetadataWithTCM name weight alpha scm =
    force DynamicCharacterMetadataDec
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
