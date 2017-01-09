-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as 
--
-----------------------------------------------------------------------------

-- TODO: Remove all commented-out code.

-- TODO: are all of these necessary?
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
-- TODO: fix and remove this ghc option (is it needed for Arbitrary?):
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Encodable.Dynamic.Internal
  ( DynamicChar (DC,Missing)
  , DynamicChars
  , DynamicCharacterElement()
  ) where

import           Bio.Character.Encodable.Dynamic.Class
import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable.Class
import           Control.Arrow                       ((***))
import           Data.Alphabet
import           Data.Bifunctor                      (bimap)
import           Data.BitMatrix
import           Data.BitMatrix.Internal(BitMatrix(..))
import           Data.Char                           (toLower)
import           Data.Key
import           Data.Bits
import           Data.BitVector               hiding (foldr, join, not, replicate)
import           Data.Foldable
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.String                         (fromString)
import           Data.Tuple                          (swap)
import           Data.Vector                         (Vector)
import           Prelude                      hiding (lookup)
import           Test.Tasty.QuickCheck        hiding ((.&.))
import           Test.QuickCheck.Arbitrary.Instances ()

--import Debug.Trace

-- TODO: Change DynamicChar/Sequences to DynamicCharacters
        -- Make a missing a null vector
        -- Think about a nonempty type class or a refinement type for this

-- |
-- Represents an encoded dynamic character, consisting of one or more static
-- characters. Dynamic characters treat entire static characters as the
-- character states of the dynamic character. The dynamic character relies on
-- the encoding of the individual static characters to defined the encoding of
-- the entire dynamic character.
data  DynamicChar
    = Missing Int
    | DC BitMatrix
    deriving (Eq, Show)


-- |
-- Represents a sinlge element of a dynamic character. 
newtype DynamicCharacterElement
      = DCE BitVector
      deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)


type instance Element DynamicChar = DynamicCharacterElement


-- | A sequence of many dynamic characters. Probably should be asserted as non-empty.
type DynamicChars = Vector DynamicChar


instance EncodedAmbiguityGroupContainer DynamicCharacterElement where

    {-# INLINE symbolCount  #-}
    symbolCount = width . unwrap


instance FiniteBits DynamicCharacterElement where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = symbolCount


instance PossiblyMissingCharacter DynamicChar where

    {-# INLINE toMissing  #-}
    toMissing c = Missing $ symbolCount c

    {-# INLINE isMissing  #-}
    isMissing Missing{} = True
    isMissing _         = False


instance EncodableStreamElement DynamicCharacterElement where

    decodeElement alphabet character = NE.fromList $ foldMapWithKey f alphabet
      where
        f i symbol
          | character `testBit` i = [symbol]
          | otherwise             = []

    -- Use foldl here to do an implicit reversal of the alphabet!
    -- The head element of the list is the most significant bit when calling fromBits.
    -- We need the first element of the alphabet to correspond to the least significant bit.
    -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
    encodeElement alphabet ambiguity = DCE . fromBits $ foldl' (\xs x -> (x `elem` ambiguity) : xs) [] alphabet


--instance NFData DynamicChar


instance MonoFunctor DynamicChar where

    {-# INLINE omap #-}
    omap f e@Missing{} = e
    omap f (DC x)      = DC . omap (unwrap . f . DCE) $ x


instance MonoFoldable DynamicChar where

    {-# INLINE ofoldMap #-}
    ofoldMap f Missing{} = mempty
    ofoldMap f (DC x)    = ofoldMap (f . DCE) $ x

    {-# INLINE ofoldr #-}
    ofoldr f e Missing{} = e
    ofoldr f e (DC x)    = ofoldr (f . DCE)  e $ x

    {-# INLINE ofoldl' #-}
    ofoldl' f e Missing{} = e
    ofoldl' f e (DC x)   = ofoldl' (\acc x -> f acc (DCE x)) e $ x

    {-# INLINE ofoldr1Ex #-} 
    ofoldr1Ex f Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an inital accumulator!"
    ofoldr1Ex f (DC x)    = DCE . ofoldr1Ex (\x y -> unwrap $ f (DCE x) (DCE y)) $ x

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an inital accumulator!"
    ofoldl1Ex' f (DC x)    = DCE . ofoldl1Ex' (\x y -> unwrap $ f (DCE x) (DCE y)) $ x

    {-# INLINE onull #-}
--    onull = const False
    onull Missing{} = True
    onull _         = False

    {-# INLINE olength #-}
    olength Missing{} = 0
    olength (DC x)    = numRows x


-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable DynamicChar where

    {-# INLINE otraverse #-}
    otraverse f e@Missing{} = pure e
    otraverse f (DC x)      = fmap DC . otraverse (fmap unwrap . f . DCE) $ x

    {-# INLINE omapM #-}
    omapM = otraverse


instance EncodedAmbiguityGroupContainer DynamicChar where

    {-# INLINE symbolCount #-}
    symbolCount (Missing n) = n
    symbolCount (DC x)     = numCols x


instance EncodableStream DynamicChar where

    decodeStream alphabet char
      | alphabet /= dnaAlphabet = rawResult 
      | otherwise               = (dnaIUPAC !) <$> rawResult
      where
        rawResult   = NE.fromList . ofoldMap (pure . decodeElement alphabet) . otoList $ char
        dnaAlphabet = fromSymbols $ fromString <$> ["A","C","G","T"]
--        dnaIUPAC :: (IsString a, Ord a) => Map [a] [a]
        dnaIUPAC    = M.fromList . fmap (swap . (NE.fromList . pure . fromChar *** NE.fromList . fmap fromChar)) $ mapping
          where
            fromChar = fromString . pure
            mapping  = gapMap <> noGapMap <> [('-', "-")]
            gapMap   = (toLower *** (<> "-")) <$> noGapMap
            noGapMap =
              [ ('A', "A"   )
              , ('C', "C"   )
              , ('G', "G"   )
              , ('T', "T"   )
              , ('M', "AC"  ) 
              , ('R', "AG"  )
              , ('W', "AT"  )
              , ('S', "CG"  )
              , ('Y', "CT"  )
              , ('K', "GT"  )
              , ('V', "ACG" )
              , ('H', "ACT" )
              , ('D', "AGT" )
              , ('B', "CGT" )
              , ('N', "ACGT")
              ]

    encodeStream alphabet = DC . fromRows . fmap (unwrap . encodeElement alphabet) . toList

    lookupStream (DC bm) i
      | 0 <= i && i < numRows bm = Just . DCE $ bm `row` i
      | otherwise                = Nothing

    {-# INLINE gapOfStream #-}
    gapOfStream = bit . pred . symbolCount 


instance EncodableDynamicCharacter DynamicChar where

    constructDynamic = DC . fromRows . fmap unwrap . toList
    
    encodeDynamic alphabet = encodeStream alphabet . NE.fromList . fmap (NE.fromList . toList) . toList


-- TODO: Probably remove?
instance Bits DynamicChar where
    (.&.)        (DC lhs) (DC rhs)  = DC $ lhs  .&.  rhs
    (.|.)        (DC lhs) (DC rhs)  = DC $ lhs  .|.  rhs
    xor          (DC lhs) (DC rhs)  = DC $ lhs `xor` rhs
    complement   (DC b)             = DC $ complement b
    shift        (DC b)   n         = DC $ b `shift`  n
    rotate       (DC b)   n         = DC $ b `rotate` n
    setBit       (DC b)   i         = DC $ b `setBit` i
    testBit      (DC b)   i         = b `testBit` i
    bit i                           = DC $ fromRows [bit i]
    bitSize                         = fromMaybe 0 . bitSizeMaybe
    bitSizeMaybe (DC b)             = bitSizeMaybe b
    isSigned     (DC b)             = isSigned b
    popCount     (DC b)             = popCount b


{-
instance Memoizable DynamicChar where
    memoize f (DC bm) = memoize (f . DC) bm
-}

instance Arbitrary DynamicCharacterElement where
    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        DCE . bitVec alphabetLen <$> (choose (1, 2 ^ alphabetLen - 1) :: Gen Integer)
      

-- We restrict the DynamicChar values generated to be non-empty.
-- Most algorithms assume a nonempty dynamic character.
instance Arbitrary DynamicChar where
    arbitrary = do 
        alphabetLen  <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
        let randVal  =  choose (1, 2 ^ alphabetLen - 1) :: Gen Integer
        bitRows      <- vectorOf characterLen randVal
        pure . DC . fromRows $ bitVec alphabetLen <$> bitRows


-- | Functionality to unencode many encoded sequences
-- decodeMany :: DynamicChars -> Alphabet -> ParsedChars
-- decodeMany seqs alph = fmap (Just . decodeOverAlphabet alph) seqs


instance Exportable DynamicChar where

    toExportableBuffer (DC bm@(BitMatrix _ bv)) = ExportableCharacterSequence x y $ bitVectorToBufferChunks x y bv
      where
        x = numRows bm
        y = numCols bm 
        
    fromExportableBuffer = undefined

    toExportableElements = encodableStreamToExportableCharacterElements
    
    fromExportableElements = DC . exportableCharacterElementsToBitMatrix 


{-
{-# INLINE unstream #-}
unstream :: DynamicChar -> BitMatrix
unstream (DC x) = x
-}

{-# INLINE unwrap #-}
unwrap :: DynamicCharacterElement -> BitVector
unwrap (DCE x) = x
