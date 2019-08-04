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

{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE UnboxedTuples         #-}

module Bio.Metadata.Dynamic.Internal
  ( DenseTransitionCostMatrix
  , DynamicCharacterMetadataDec()
  , DynamicCharacterMetadata(..)
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , MemoizedCostMatrix()
  , TraversalFoci
  , TraversalFocusEdge
  , TraversalTopology
--  , TransitionCostMatrix
  , dynamicMetadata
  , dynamicMetadataFromTCM
  , dynamicMetadataWithTCM
  , maybeConstructDenseTransitionCostMatrix
  , overlap
  , overlap'
  ) where


import           Bio.Character.Encodable
import           Bio.Metadata.CharacterName
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic.Class   hiding (DenseTransitionCostMatrix)
import           Control.DeepSeq
import           Control.Lens                 hiding (Fold)
import           Control.Monad.State.Strict
import           Data.Alphabet
import           Data.Bits
import           Data.FileSource
import           Data.Foldable
import           Data.Functor                 (($>))
import           Data.Hashable
import           Data.Hashable.Memoize
import           Data.List                    (intercalate)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Maybe
import           Data.MetricRepresentation
import           Data.Ord
import           Data.Range
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.TCM                     hiding ((!), size)
import qualified Data.TCM                     as TCM
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           Data.TopologyRepresentation
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (lookup)
import           Text.XML
import           Data.BitVector.LittleEndian

import Control.DeepSeq
import Data.Semigroup (Min(..))


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


{-
-- |
-- A generalized function representation: the "overlap" between dynamic character
-- elements, supplying the corresponding median and cost to align the two
-- characters.
type PairwiseTransitionCostMatrix e = e -> e -> (e, Word)
-}


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets.
data DynamicCharacterMetadataDec c
   = DynamicCharacterMetadataDec
   { optimalTraversalFoci        :: !(Maybe TraversalFoci)
   , structuralRepresentationTCM :: !(Either
                                        (DenseTransitionCostMatrix, MetricRepresentation ())
                                        (MetricRepresentation MemoizedCostMatrix)
   {-                                   TODO: try to change to this
                                        (MetricRepresentation (c -> c -> (c, Word)))
   -}
                                     )
   , metadata                    :: {-# UNPACK #-} !DiscreteCharacterMetadataDec
   } deriving (Generic, NFData)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata s c
      , GetDenseTransitionCostMatrix     s (Maybe DenseTransitionCostMatrix)
      , GetSparseTransitionCostMatrix    s (Maybe MemoizedCostMatrix)
      , GetPairwiseTransitionCostMatrix  s c Word
      , GetThreewayTransitionCostMatrix  s (c -> c -> c -> (c, Word))
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
instance (Bound c ~ Word, EncodableStreamElement c, Hashable c, NFData c, Ranged c)
    => DiscreteWithTcmCharacterMetadata (DynamicCharacterMetadataDec c) c where


-- | (✔)
instance (Bound c ~ Word, EncodableStreamElement c, Hashable c, NFData c, Ranged c)
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

    denseTransitionCostMatrix = to
      $ either (Just . fst) (const Nothing) . structuralRepresentationTCM

-- | (✔)
instance HasTcmSourceFile (DynamicCharacterMetadataDec c) FileSource where

    _tcmSourceFile = lens (\d -> metadata d ^. _tcmSourceFile)
                   $ \d s -> d { metadata = metadata d & _tcmSourceFile .~ s }

-- | (✔)
instance GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec c) (Maybe MemoizedCostMatrix) where

    sparseTransitionCostMatrix = to $
       either (const Nothing) (foldl' (const Just) Nothing) . structuralRepresentationTCM


-- | (✔)
instance GetSymbolChangeMatrix (DynamicCharacterMetadataDec c) (Word -> Word -> Word) where

    symbolChangeMatrix = to
      $ retreiveSCM . either snd void . structuralRepresentationTCM


-- | (✔)
instance (Bound c ~ Word, EncodableStreamElement c, Hashable c, NFData c, Ranged c)
    => GetPairwiseTransitionCostMatrix (DynamicCharacterMetadataDec c) c Word where

    pairwiseTransitionCostMatrix = to extractPairwiseTransitionCostMatrix


-- | (✔)
instance (Bound c ~ Word, EncodableStreamElement c, Hashable c, NFData c, Ranged c)
    => GetThreewayTransitionCostMatrix (DynamicCharacterMetadataDec c) (c -> c -> c -> (c, Word)) where

    threewayTransitionCostMatrix = to extractThreewayTransitionCostMatrix


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
  -> FileSource
  -> TCM
  -> Maybe DenseTransitionCostMatrix -> DynamicCharacterMetadataDec c
dynamicMetadata name weight alpha tcmSource tcm denseMay =
    force DynamicCharacterMetadataDec
    { optimalTraversalFoci        = Nothing
    , structuralRepresentationTCM = representaionOfTCM
    , metadata                    = discreteMetadata name weight alpha tcmSource
    }
  where
    representaionOfTCM = maybe largeAlphabet smallAlphabet denseMay
      where
        largeAlphabet   = Right $ metricRep $> memoMatrixValue
        smallAlphabet x = Left (x,metricRep)

    metricRep =
        case tcmStructure diagnosis of
          NonAdditive -> DiscreteMetric
          Additive    -> LinearNorm
          _           -> ExplicitLayout (factoredTcm diagnosis) ()

    diagnosis       = diagnoseTcm tcm
--    coefficient     = fromIntegral $ factoredWeight diagnosis
    sigma  i j      = toEnum . fromEnum $ factoredTcm diagnosis TCM.! (fromEnum i, fromEnum j)
    memoMatrixValue = generateMemoizedTransitionCostMatrix (toEnum $ length alpha) sigma


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataFromTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> FileSource
  -> TCM
  -> DynamicCharacterMetadataDec c
dynamicMetadataFromTCM name weight alpha tcmSource tcm
    = dynamicMetadata name weight alpha tcmSource tcm denseMay
  where
    denseMay = maybeConstructDenseTransitionCostMatrix alpha (\i j -> toEnum . fromEnum $ tcm TCM.! (i,j))


-- |
-- Construct a concrete typed 'DynamicCharacterMetadataDec' value from the supplied inputs.
dynamicMetadataWithTCM
  :: CharacterName
  -> Double
  -> Alphabet String
  -> FileSource
  -> (Word -> Word -> Word)
  -> DynamicCharacterMetadataDec c
dynamicMetadataWithTCM name weight alpha tcmSource scm =
    dynamicMetadataFromTCM name weight alpha tcmSource tcm
  where
    tcm = generate (length alpha) (uncurry scm)


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
    f | len > 8   = Nothing
      | otherwise = Just $ generateDenseTransitionCostMatrix 0 len sigma
      where
        len = toEnum $ length alpha


-- |
-- /O(n^2)/
--
-- Correctly select the most efficient TCM function based on the alphabet size
-- and metric specification.
{-# INLINE extractPairwiseTransitionCostMatrix #-}
{-# SPECIALISE extractPairwiseTransitionCostMatrix :: DynamicCharacterMetadataDec DynamicCharacterElement -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
extractPairwiseTransitionCostMatrix
  :: ( EncodableStreamElement c
     , Hashable c
     , NFData c
     , Ranged c
     , Bound c ~ Word
     )
  => DynamicCharacterMetadataDec c2
  -> c
  -> c
  -> (c, Word)
extractPairwiseTransitionCostMatrix =
    either
      (lookupPairwise . fst)
      (retreivePairwiseTCM (const . buildMemo2))
    . structuralRepresentationTCM
{-
  where
--getMedianAndCost2D memo
extractPairwiseTransitionCostMatrix = retreivePairwiseTCM handleGeneralCases . structuralRepresentationTCM
  where
    handleGeneralCases scm v =
        case v of
          Left  dense -> lookupPairwise dense
          Right _memo -> memoize2 $ overlap2 (\i j -> toEnum . fromEnum $ scm TCM.! (fromEnum i, fromEnum j)) --getMedianAndCost2D memo
-}


{-# SPECIALISE buildMemo2 :: TCM -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
buildMemo2
  :: ( EncodableStreamElement c
     , Hashable c
     , NFData c
     )
  => TCM -> c -> c -> (c, Word)
buildMemo2 scm = memoize2 $ overlap2 (\i j -> toEnum . fromEnum $ scm TCM.! (fromEnum i, fromEnum j))


-- |
-- /O(n^2)/
--
-- Correctly select the most efficient TCM function based on the alphabet size
-- and metric specification.
{-# INLINE extractThreewayTransitionCostMatrix #-}
{-# SPECIALISE extractThreewayTransitionCostMatrix :: DynamicCharacterMetadataDec DynamicCharacterElement -> DynamicCharacterElement -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
extractThreewayTransitionCostMatrix
  :: ( EncodableStreamElement c
     , Hashable c
     , NFData c
     , Ranged c
     , Bound c ~ Word
     )
  => DynamicCharacterMetadataDec c2
  -> c
  -> c
  -> c
  -> (c, Word)
extractThreewayTransitionCostMatrix =
  either
    (lookupThreeway . fst)
    (retreiveThreewayTCM memoed)
  . structuralRepresentationTCM
  where
    memoed scm _ = memoize3 $ overlap3 (\i j -> toEnum . fromEnum $ scm TCM.! (fromEnum i, fromEnum j)) --getMedianAndCost3D memo


-- |
-- Takes one or more elements of 'FiniteBits' and a symbol change cost function
-- and returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function.
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
{-# INLINE overlap #-}
{-# SPECIALISE overlap :: FiniteBits e => (Word -> Word -> Word) -> NonEmpty e -> (e, Word) #-}
{-# SPECIALISE overlap :: (Word -> Word -> Word) -> NonEmpty DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap
  :: forall e f.
     ( FiniteBits e
     , Foldable1 f
     , Functor f
     )
  => (Word -> Word -> Word) -- ^ Symbol change matrix (SCM) to determin cost
  -> f e                    -- ^ List of elements for of which to find the k-median and cost
  -> (e, Word)              -- ^ K-median and cost
overlap sigma xs = go size maxBound zero
  where
    (size, zero) = let wlog = getFirst $ foldMap1 First xs
                   in  (finiteBitSize wlog, wlog `xor` wlog)

    go :: Int -> Word -> e -> (e, Word)
    go 0 theCost bits = (bits, theCost)
    go i oldCost bits =
        let newCost = sum $ getDistance (toEnum i) <$> xs
            (minCost, bits') = case oldCost `compare` newCost of
                                 EQ -> (oldCost, bits `setBit` i)
                                 LT -> (oldCost, bits           )
                                 GT -> (newCost, zero `setBit` i)
        in go (i-1) minCost bits'

    getDistance i b = go' size (maxBound :: Word)
      where
        go' :: Int -> Word -> Word
        go' 0 a = a
        go' j a =
          let a' =
                if b `testBit` j then min a $ sigma i (toEnum j)
                else a
                  in  go' (j-1) a'


overlap'
  :: forall  f.
     ( Foldable1 f
     , Functor f
     , NFData (f DynamicCharacterElement)
     )
  => (Word -> Word -> Word)
  -> f DynamicCharacterElement
  -> (DynamicCharacterElement, Word)
overlap' sigma xs = go size maxBound zero
  where
    (size, !zero) = let !wlog = getFirst $ foldMap1 First xs
                   in  (finiteBitSize wlog, wlog `xor` wlog)
    size'  = size - 1

  --  go :: Int -> Word -> e -> (e, Word)
    go 0 theCost bits = (bits, theCost)
    go i oldCost bits =
        let !newCost = sum $ getDistance (toEnum i) <$>  xs
            (minCost, bits') = case oldCost `compare` newCost of
                                 EQ -> (oldCost, bits `setBit` i)
                                 LT -> (oldCost, bits           )
                                 GT -> (newCost, zero `setBit` i)
        in go (i-1) minCost bits'

    getDistance i !b = go' 0 0 (maxBound :: Word)
      where
        go' :: Int -> Word -> Word -> Word
        go' j _ a | j == size' = a
        go' j o a =
          let
            o' = o + 1
            (a', j') =
                if b `testBit` j then (min a $ sigma i (toEnum j), j + 1)
                else
                  case selectDC b o' of
                    Just !ind ->  (min a $ sigma i ind, (fromEnum ind) + 1)
                    _        ->  (a, size')
                  in  go' j' o' a'


{-# INLINE overlap2 #-}
{-# SPECIALISE overlap2 :: (Word -> Word -> Word) -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap2
  :: (EncodableStreamElement e {- , Show e -})
  => (Word -> Word -> Word)
  -> e
  -> e
  -> (e, Word)
overlap2 sigma char1 char2 = overlap sigma $ char1 :| [char2]


{-# INLINE overlap3 #-}
{-# SPECIALISE overlap3 :: (Word -> Word -> Word) -> DynamicCharacterElement -> DynamicCharacterElement -> DynamicCharacterElement -> (DynamicCharacterElement, Word) #-}
overlap3
  :: (EncodableStreamElement e {- , Show e -})
  => (Word -> Word -> Word)
  -> e
  -> e
  -> e
  -> (e, Word)
overlap3 sigma char1 char2 char3 = overlap sigma $ char1 :| [char2, char3]
