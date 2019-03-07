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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedSums           #-}

module Bio.Metadata.DiscreteWithTCM.Internal
  ( DiscreteCharacterMetadata(..)
  , DiscreteWithTCMCharacterMetadataDec()
  , GeneralCharacterMetadata(..)
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , discreteMetadataFromTCM
  , discreteMetadataWithTCM
  ) where

import Bio.Character.Exportable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM.Class
import Bio.MetricRepresentation
import Bio.TCM                            as TCM
import Bio.TCM.Memoized
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.Bits
import Data.List                          (intercalate)
import Data.Range
import Data.Text.Short                    (ShortText)
import GHC.Generics                       hiding (to)
import Text.XML


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets.
data DiscreteWithTCMCharacterMetadataDec c
   = DiscreteWithTCMCharacterMetadataDec
   { metricRepresentation :: !(MetricRepresentation MemoizedCostMatrix)
   , discreteData         :: {-# UNPACK #-} !DiscreteCharacterMetadataDec
   } deriving (Eq, Generic)


foreignPointerData :: DiscreteWithTCMCharacterMetadataDec c -> Maybe MemoizedCostMatrix
foreignPointerData x =
  case metricRepresentation x of
    ExplicitLayout _ v -> Just v
    _                  -> Nothing


{-
data RepresentedTCM a where
  Disc :: Discrete -> RepresentedTCM Discrete
  Explicit :: TCM -> MemoizedCostMatrix -> RepresentedTCM Explicit
-}


{-
retreiveTCM
  :: ( Bits c
     , Bound c ~ Word
     , Exportable c
     , Ranged c
     )
  => RepresentedTCM
  -> c
  -> c
  -> (c, Word)
retreiveTCM (ExplicitLayout _ memo) = getMedianAndCost2D memo
retreiveTCM DiscreteMetric          = discreteMetricLogic
retreiveTCM LinearNorm              = linearNormLogic


retreiveSCM :: RepresentedTCM -> Word -> Word -> Word
retreiveSCM (ExplicitLayout tcm _) = \i j -> toEnum . fromEnum $ tcm TCM.! (i,j)
retreiveSCM DiscreteMetric         = \i j -> if i == j then 0 else 1
retreiveSCM LinearNorm             = \i j -> max i j - min i j


discreteMetricLogic :: (Bits a, Num b) => a -> a -> (a, b)
discreteMetricLogic lhs rhs
  | popCount intersect > 0 = (intersect, 0)
  | otherwise              = (  unioned, 1)
  where
    unioned   = lhs .|. rhs
    intersect = lhs .&. rhs


linearNormLogic
  :: ( Ord (Bound a)
     , Ranged a
     , Ranged b
     , Ranged c
     , Bound b ~ Bound a
     , Bound c ~ Bound a
     )
  => a -> b -> (c, Bound a)
linearNormLogic lhs rhs = (fromRange newInterval, cost)
  where
    lhs' = toRange lhs
    rhs' = toRange rhs

    newInterval
      | isOverlapping = lhs' `intersection`   rhs'
      | otherwise     = lhs' `smallestClosed` rhs'
    isOverlapping     = lhs' `intersects`     rhs'

    cost
      | isOverlapping = 0
      | otherwise     = upperBound newInterval - lowerBound newInterval
-}


-- | (✔)
instance DiscreteCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = discreteData


-- | (✔)
instance HasCharacterAlphabet (DiscreteWithTCMCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens (\e -> discreteData e ^. characterAlphabet)
                      $ \e x -> e { discreteData = discreteData e & characterAlphabet .~ x }


-- | (✔)
instance GeneralCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata =  extractGeneralCharacterMetadata . discreteData


-- | (✔)
instance HasCharacterName (DiscreteWithTCMCharacterMetadataDec c) CharacterName where

    characterName = lens (\e -> discreteData e ^. characterName)
                  $ \e x -> e { discreteData = discreteData e & characterName .~ x }


-- | (✔)
instance HasCharacterWeight (DiscreteWithTCMCharacterMetadataDec c) Double where

    characterWeight = lens (\e -> discreteData e ^. characterWeight)
                    $ \e x -> e { discreteData = discreteData e & characterWeight .~ x }

-- | (✔)
instance HasTcmSourceFile (DiscreteWithTCMCharacterMetadataDec c) ShortText where

    _tcmSourceFile = lens (\s -> discreteData s ^. _tcmSourceFile)
                   $ \d s -> d { discreteData = discreteData d & _tcmSourceFile .~ s }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance GetSparseTransitionCostMatrix (DiscreteWithTCMCharacterMetadataDec c) (Maybe MemoizedCostMatrix) where

    sparseTransitionCostMatrix = to foreignPointerData


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance GetSymbolChangeMatrix (DiscreteWithTCMCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = to (retreiveSCM . metricRepresentation)


-- |
-- A 'Lens' for the 'pairwiseTransitionCostMatrix' field
instance (Bits c, Bound c ~ Word, Exportable c, Ranged c)
    => GetPairwiseTransitionCostMatrix (DiscreteWithTCMCharacterMetadataDec c) (c -> c -> (c, Word)) where

    pairwiseTransitionCostMatrix = to (retreivePairwiseTCM (const getMedianAndCost2D) . metricRepresentation)


instance NFData (DiscreteWithTCMCharacterMetadataDec c) where

    rnf val = rnf (metricRepresentation val)
        `seq` rnf (discreteData   val)
        `seq` ()


instance Show (DiscreteWithTCMCharacterMetadataDec c) where

    show e = intercalate "\n"
        [ "DiscreteCharacterMetadata"
        , "  CharacterName: " <> show (e ^. characterName    )
        , "  Alphabet:      " <> show (e ^. characterAlphabet)
        , "  Weight:        " <> show (e ^. characterWeight  )
        , "  TCMSourceFile :" <> show (e ^. _tcmSourceFile   )
        , "  TCM: "
        , show . generate dimension $ \(i,j) -> cost (toEnum i) (toEnum j)
        ]
      where
        cost      = e ^. symbolChangeMatrix
        dimension = length $ e ^. characterAlphabet


instance ToXML (DiscreteWithTCMCharacterMetadataDec c) where

    toXML input = xmlElement "Discrete_with_TCM" attrs contents
        where
            attrs    = []
            contents = [ Right . toXML $ discreteData input
                       , Left ("TCM", "Not yet renderable")
                       ]


-- |
-- Construct a concrete typed 'DiscreteWithTCMCharacterMetadataDec' value from the supplied inputs.
discreteMetadataFromTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> ShortText
  -> TCM
  -> DiscreteWithTCMCharacterMetadataDec c
discreteMetadataFromTCM name weight alpha tcmSource tcm =
    DiscreteWithTCMCharacterMetadataDec
    { metricRepresentation = representaionOfTCM
    , discreteData   = discreteMetadata name (weight * coefficient) alpha tcmSource
    }
  where
    representaionOfTCM =
        case tcmStructure diagnosis of
          NonAdditive -> DiscreteMetric
          Additive    -> LinearNorm
          _           -> ExplicitLayout (factoredTcm diagnosis) memoMatrixValue

    diagnosis       = diagnoseTcm tcm
    coefficient     = fromIntegral $ factoredWeight diagnosis
    sigma  i j      = toEnum . fromEnum $ factoredTcm diagnosis ! (fromEnum i, fromEnum j)
    memoMatrixValue = generateMemoizedTransitionCostMatrix (toEnum $ length alpha) sigma


-- |
-- Construct a concrete typed 'DiscreteWithTCMCharacterMetadataDec' value from the supplied inputs.
discreteMetadataWithTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> ShortText
  -> (Word -> Word -> Word)
  -> DiscreteWithTCMCharacterMetadataDec c
discreteMetadataWithTCM name weight alpha tcmSource scm = discreteMetadataFromTCM name weight alpha tcmSource tcm
  where
    tcm = generate (length alpha) (uncurry scm)
