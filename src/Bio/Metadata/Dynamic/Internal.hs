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

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilies           #-}

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
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Metadata.CharacterName
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic.Class
import           Bio.Metadata.MetricRepresentation
import           Control.DeepSeq
import           Control.Lens                      hiding (Fold)
import           Control.Monad.State.Strict
import           Data.Alphabet
import           Data.Foldable
import           Data.Functor                      (($>))
import           Data.List                         (intercalate)
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Maybe
import           Data.Ord
import           Data.Range
import           Data.Semigroup
import           Data.TCM
import qualified Data.TCM                          as TCM
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           Data.Text.Short                   (ShortText)
import           Data.TopologyRepresentation
import           GHC.Generics                      (Generic)
import           Prelude                           hiding (lookup)
import           Text.XML



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
                                     )
   , metadata                    :: {-# UNPACK #-} !DiscreteCharacterMetadataDec
   } deriving (Generic, NFData)


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata s c
      , GetDenseTransitionCostMatrix     s (Maybe DenseTransitionCostMatrix)
      , GetSparseTransitionCostMatrix    s (Maybe MemoizedCostMatrix)
      , GetPairwiseTransitionCostMatrix  s (     c -> c -> (c, Word))
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
instance (Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => DiscreteWithTcmCharacterMetadata (DynamicCharacterMetadataDec c) c where


-- | (✔)
instance (Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
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
instance HasTcmSourceFile (DynamicCharacterMetadataDec c) ShortText where

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
instance (Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
    => GetPairwiseTransitionCostMatrix (DynamicCharacterMetadataDec c) (c -> c -> (c, Word)) where

    pairwiseTransitionCostMatrix = to extractPairwiseTransitionCostMatrix


-- | (✔)
instance (Bound c ~ Word, EncodableStreamElement c, Exportable c, Ranged c)
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
  -> ShortText
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
  -> ShortText
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
  -> ShortText
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
extractPairwiseTransitionCostMatrix
  :: ( Exportable c
     , EncodableStreamElement c
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
    (retreivePairwiseTCM (const getMedianAndCost2D))
  . structuralRepresentationTCM


-- |
-- /O(n^2)/
--
-- Correctly select the most efficient TCM function based on the alphabet size
-- and metric specification.
extractThreewayTransitionCostMatrix
  :: ( Exportable c
     , EncodableStreamElement c
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
    (retreiveThreewayTCM (const getMedianAndCost3D))
  . structuralRepresentationTCM


{-
-- |
-- An overlap function that applies the discrete metric to aligning two elements.
discreteOverlap :: EncodableStreamElement e => e -> e -> (e, Word)
discreteOverlap lhs rhs
  | intersect == zeroBits = (lhs .|. rhs, 1)
  | otherwise             = (intersect  , 0)
  where
    intersect = lhs .&. rhs


-- |
-- Takes two 'EncodableStreamElement' and a symbol change cost function and
-- returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function.
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
overlap :: (EncodableStreamElement e {- , Show e -}) => (Word -> Word -> Word) -> e -> e -> (e, Word)
overlap costStruct char1 char2
  | intersectionStates == zero = deriveOverlap costStruct char1 char2 -- minimalChoice $ symbolDistances costStruct char1 char2
  | otherwise                  = (intersectionStates, 0)
  where
    intersectionStates = char1 .&. char2
    zero = char1 `xor` char1


deriveOverlap
  :: EncodableStreamElement e
  => (Word -> Word -> Word)
  -> e
  -> e
  -> (e, Word)
deriveOverlap costStruct char1 char2 = F.fold
    (F.premap (costAndSymbol . (toEnum &&& setBit zero)) outerFold)
    symbolIndices
  where
    outerFold = Fold f (char1 `xor` char1, maxBound) id

    f (!symbol1, !cost1) (!symbol2, !cost2) =
        case cost1 `compare` cost2 of
          EQ -> (symbol1 .|. symbol2, cost1)
          LT -> (symbol1            , cost1)
          GT -> (symbol2            , cost2)

    costAndSymbol (i, x) = (x, cost1 + cost2)
      where
        !cost1 = getDistance3 i char1
        !cost2 = getDistance3 i char2

    symbolIndices = NE.fromList [0 .. finiteBitSize char1 - 1]
    zero          = char1 `xor` char1

    getDistance3 :: (MonoFoldable b, Element b ~ Bool) => Word -> b -> Word
    getDistance3 i b = fromMaybe errMsg $
        F.impurely ofoldMUnwrap (F.prefilterM pure (F.premapM g (F.generalize F.minimum))) b `evalState` 0
      where
        errMsg = error "There were no bits set in the character!"

        g _ = do
            j <- get
            modify' (+1)
            pure $ costStruct i j
{-
    getDistance2 :: FiniteBits b => Word -> b -> Word
    getDistance2 i b =
        case F.fold (F.prefilter (b `testBit`) (F.premap (costStruct i . toEnum) F.minimum)) indices of
          Just x  -> x
          Nothing -> error $ "There were no bits set in the character!"
      where
        indices = [0 .. finiteBitSize b - 1]
-}
-}
