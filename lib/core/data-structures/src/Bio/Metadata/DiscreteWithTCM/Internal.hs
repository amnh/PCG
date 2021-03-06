-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.DiscreteWithTCM.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
  ) where

import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM.Class
import Bio.Metadata.Overlap
import Control.Applicative
import Control.DeepSeq
import Control.Lens.Combinators           (lens, to)
import Control.Lens.Operators             ((&), (.~), (^.))
import Data.Alphabet
import Data.Binary
import Data.Bits
import Data.CharacterName
import Data.FileSource
import Data.Functor
import Data.Hashable
import Data.Hashable.Memoize
import Data.List                          (intercalate)
import Data.MetricRepresentation
import Data.Range
import Data.TCM                           as TCM
import Data.TCM.Memoized
import GHC.Generics                       hiding (to)
import Text.XML


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continuous bins do not have Alphabets.
data  DiscreteWithTCMCharacterMetadataDec c
    = DiscreteWithTCMCharacterMetadataDec
    { metricRepresentation :: !(MetricRepresentation ( MemoizedCostMatrix
                                                     , c -> c -> (c, Word)
                                                     , c -> c -> c -> (c, Word))
                                                     )
    , discreteData         :: {-# UNPACK #-} !DiscreteCharacterMetadataDec
    }
    deriving stock (Generic)


foreignPointerData :: DiscreteWithTCMCharacterMetadataDec c -> Maybe MemoizedCostMatrix
foreignPointerData x =
    case metricRepresentation x of
      ExplicitLayout _ (v,_,_) -> Just v
      _                        -> Nothing


instance
     ( Eq c
     , FiniteBits c
     , Hashable c
     , NFData c
     ) => Binary (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE put #-}
    put x = put (void (metricRepresentation x)) <> put (discreteData x)

    {-# INLINE get #-}
    get   = liftA2 DiscreteWithTCMCharacterMetadataDec (rebuildMetricRepresentation <$> get) get


rebuildMetricRepresentation
  :: ( FiniteBits c
     , Hashable c
     , NFData c
     )
  => MetricRepresentation ()
  -> MetricRepresentation ( MemoizedCostMatrix
                          , c -> c -> (c, Word)
                          , c -> c -> c -> (c, Word)
                          )
rebuildMetricRepresentation metricRep =
    case metricRep of
      DiscreteMetric       -> DiscreteMetric
      LinearNorm           -> LinearNorm
      ExplicitLayout tcm _ ->
          let     scm i j = toEnum . fromEnum $ tcm TCM.! (fromEnum i, fromEnum j)
                  len     = toEnum $ TCM.size tcm
          in  ExplicitLayout tcm ( generateMemoizedTransitionCostMatrix len scm
                                 , memoize2 $ overlap2 scm
                                 , memoize3 $ overlap3 scm
                                 )


instance DiscreteCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractDiscreteCharacterMetadata #-}
    extractDiscreteCharacterMetadata = discreteData


instance Eq (DiscreteWithTCMCharacterMetadataDec c) where

    (==) lhs rhs = (discreteData lhs == discreteData rhs) &&
                       (metricRepresentation lhs $> ()) == (metricRepresentation rhs $> ())


instance HasCharacterAlphabet (DiscreteWithTCMCharacterMetadataDec c) (Alphabet String) where

    characterAlphabet = lens (\e -> discreteData e ^. characterAlphabet)
                      $ \e x -> e { discreteData = discreteData e & characterAlphabet .~ x }


instance GeneralCharacterMetadata (DiscreteWithTCMCharacterMetadataDec c) where

    {-# INLINE extractGeneralCharacterMetadata #-}
    extractGeneralCharacterMetadata =  extractGeneralCharacterMetadata . discreteData


instance HasCharacterName (DiscreteWithTCMCharacterMetadataDec c) CharacterName where

    characterName = lens (\e -> discreteData e ^. characterName)
                  $ \e x -> e { discreteData = discreteData e & characterName .~ x }


instance HasCharacterWeight (DiscreteWithTCMCharacterMetadataDec c) Double where

    characterWeight = lens (\e -> discreteData e ^. characterWeight)
                    $ \e x -> e { discreteData = discreteData e & characterWeight .~ x }

instance HasTcmSourceFile (DiscreteWithTCMCharacterMetadataDec c) FileSource where

    _tcmSourceFile = lens (\s -> discreteData s ^. _tcmSourceFile)
                   $ \d s -> d { discreteData = discreteData d & _tcmSourceFile .~ s }


instance GetSparseTransitionCostMatrix (DiscreteWithTCMCharacterMetadataDec c) (Maybe MemoizedCostMatrix) where

    sparseTransitionCostMatrix = to foreignPointerData


instance GetSymbolChangeMatrix (DiscreteWithTCMCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = to (retreiveSCM . metricRepresentation)


instance (Bits c, Bound c ~ Word, Ranged c)
--instance (Bits c, Bound c ~ Word, ExportableBuffer c, Ranged c)
    => GetPairwiseTransitionCostMatrix (DiscreteWithTCMCharacterMetadataDec c) c Word where

    pairwiseTransitionCostMatrix =
        to (retreivePairwiseTCM . fmap (\(_, x, _) -> x) . metricRepresentation)


instance NFData (DiscreteWithTCMCharacterMetadataDec c) where

    rnf val = rnf (metricRepresentation val) `seq` rnf (discreteData   val)


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
  :: ( FiniteBits c
     , Hashable c
     , NFData c
     )
  => CharacterName
  -> Double
  -> Alphabet String
  -> FileSource
  -> TCM
  -> DiscreteWithTCMCharacterMetadataDec c
discreteMetadataFromTCM name weight alpha tcmSource tcm' =
    DiscreteWithTCMCharacterMetadataDec
    { metricRepresentation = representaionOfTCM
    , discreteData   = discreteMetadata name (weight * coefficient) alpha tcmSource
    }
  where
    representaionOfTCM =
        case tcmStructure diagnosis of
          NonAdditive -> DiscreteMetric
          Additive    -> LinearNorm
          _           -> ExplicitLayout tcm
                           (memoMatrixValue, memoize2 $ overlap2 scm, memoize3 $ overlap3 scm)

    scm i j         = toEnum . fromEnum $ tcm TCM.! (fromEnum i, fromEnum j)
    tcm             = factoredTcm diagnosis
    diagnosis       = diagnoseTcm tcm'
    coefficient     = fromIntegral $ factoredWeight diagnosis
    sigma i j       = toEnum . fromEnum $ tcm ! (fromEnum i, fromEnum j)
    memoMatrixValue = generateMemoizedTransitionCostMatrix (toEnum $ length alpha) sigma
