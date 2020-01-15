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

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UnboxedSums                #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bio.Character.Encodable.Dynamic.Element
  ( DynamicCharacterElement(..)
  , Subcomponent
  , arbitraryOfSize
  , getLeft
  , getRight
  , packDynamicCharacterElement
  ) where

import           Bio.Character.Encodable.Dynamic.AmbiguityGroup
import           Bio.Character.Encodable.Dynamic.Class
import           Bio.Character.Encodable.Internal
import           Bio.Character.Exportable              (Subcomponent)
import           Control.Applicative
import           Control.DeepSeq
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Coerce
import           Data.Foldable
import           Data.Hashable
import           Data.MonoTraversable
import           Data.Range
import           Data.String                           (fromString)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances   ()
import           TextShow                              (TextShow (showb), toString)

import Debug.Trace


-- |
-- Represents a single element of a dynamic character, which holds two
-- 'AmbiguityGroup' compoents and is tagged with an "alignment context,"
-- describing the relationship between the two components.
--
-- *NOTE:* That the 'DynamicCharacterElement' will require a number of bits equal
-- to /twice/ the size of the alphabet to represent the power-set of possible
-- symbols for two 'AmbiguityGroup's. 'AmbiguityGroup' values must be non-empty.
-- This means that a valid 'AmbiguityGroup' value has at least one bit set. We
-- utilize this fact as part of our 'DynamicCharacterElement' encoding, decribed
-- below. 
--
-- There he four alignment contexts the element has can be tagegd as: 
-- @(GAPPING, DELETETION, INSERTION, or ALIGNMENT)@.
--
-- * @GAPPING@ represents /neither/ of the two possible 'AmbiguityGroup' values
-- being stored 'DynamicCharacterElement'. The 'DynamicCharacterElement' has
-- a zero vector as it's bit representation.
--
-- * @INSERTION@ represents just the /second/ of the two possible 'AmbiguityGroup'
-- values being stored 'DynamicCharacterElement'. The 'DynamicCharacterElement'
-- has a zero vector in the /first/ half of it's bit representation and the
-- 'AmbiguityGroup' value packed into the /second/ half of it's bit representation.
--
-- * @DELETION@ represents just the /first/ of the two possible 'AmbiguityGroup'
-- values being stored 'DynamicCharacterElement'. The 'DynamicCharacterElement'
-- has the 'AmbiguityGroup' value packed into the /first/ half of it's bit
-- representation and a zero vector in the /second/ half of it's bit representation.
--
-- * @ALIGNMENT@ represents /both/ 'AmbiguityGroup' values being stored in the
-- 'DynamicCharacterElement'. The 'DynamicCharacterElement' has both 'AmbiguityGroup'
-- values packed consecutively in it's bitrepresentation.
--
-- The encoding scheme presented above represents an optimal-space encoding for our
-- data-type.
newtype DynamicCharacterElement
      = DCE BitVector
      deriving stock   (Generic)
      deriving newtype (Bits, Eq, FiniteBits, Hashable, NFData, Ord)


type instance Bound DynamicCharacterElement = Word


type instance Subcomponent DynamicCharacterElement = AmbiguityGroup


instance Arbitrary DynamicCharacterElement where

    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        arbitraryOfSize $ toEnum alphabetLen


instance CoArbitrary DynamicCharacterElement


instance EncodedAmbiguityGroupContainer DynamicCharacterElement where

    {-# INLINE symbolCount  #-}
    symbolCount = (`div` 2) . dimension . packDynamicCharacterElement


instance EncodableDynamicCharacterElement DynamicCharacterElement where
  
    isGap      = isZeroVector . packDynamicCharacterElement

    isInsert x =      isLeftEmpty x  && not (isRightEmpty x)

    isDelete x = not (isLeftEmpty x) &&     (isRightEmpty x)

    isAlign  x = not (isLeftEmpty x) && not (isRightEmpty x)
    
    gapElement w                = DCE $ fromNumber (w `shiftL` 1) (0 :: Word)

    deleteElement        (AG y) = DCE $ zeroVectorOf y <> y

    insertElement (AG x)        = DCE $ x <> zeroVectorOf x

    alignElement  (AG x) (AG y) = DCE $ x <> y

    getContext dce =
        case (not $ isLeftEmpty dce, not $ isRightEmpty dce) of
          (False, False) -> Gapping
          (False, True ) -> Deletion
          (True , False) -> Insertion
          (True , True ) -> Alignment

    swapContext dce =
      let lhs = coerce $ getLeft  dce
          rhs = coerce $ getRight dce
      in  DCE $ rhs <> lhs 


--  getMedian     :: (Subcomponent e -> Subcomponent e -> Subcomponent e) -> e -> Subcomponent e
    getMedian transform dce =
        let gap = AG . bit . fromEnum . pred
--                  . (\x -> trace (unlines [ "In 'gap' creation of getMedian,"
--                                          , "DCE: " <> show dce
--                                          , "symbolCount " <> show (symbolCount dce)
--                                          ]) x)
                    $ symbolCount dce
            one = {- (\x -> trace ("one " <> show x) x) . -} getLeft  dce
            two = {- (\x -> trace ("two " <> show x) x) . -} getRight dce
        in  -- (\x -> trace ("From input: " <> show dce <> "\nMedian got: " <> show x) x) $
            case getContext dce of
                Gapping   -> gap
                Deletion  -> transform gap two
                Insertion -> transform one gap
                Alignment -> transform one two


{-
instance EncodableStreamElement DynamicCharacterElement where

    decodeElement alphabet character =
        case foldMapWithKey f alphabet of
          []   -> error "Attempting to decode an empty dynamic character element."
          x:xs -> x:|xs
      where
        f i symbol
          | character `testBit` i = [symbol]
          | otherwise             = []

    -- Use foldl here to do an implicit reversal of the alphabet!
    -- The head element of the list is the most significant bit when calling fromBits.
    -- We need the first element of the alphabet to correspond to the least significant bit.
    -- Hence foldl, don't try foldMap or toList & fmap without careful thought.
    encodeElement alphabet ambiguity = DCE . fromBits $ (`elem` ambiguity) <$> toList alphabet
-}


{--
instance Enum DynamicCharacterElement where

    fromEnum = toUnsignedNumber . packDynamicCharacterElement

    toEnum i = DCE $ fromNumber dim i
      where
        dim = toEnum $ finiteBitSize i - countLeadingZeros i
--}


instance Show DynamicCharacterElement where

    show = toString . showb


instance TextShow DynamicCharacterElement where

    showb dce@(DCE bv) = fold ["[", prefix, "|", fromString (drop 2 (ofoldMap g bv)), "] ", showb bv]
      where
        g b | b         = "1"
            | otherwise = "0"
        
        prefix = case (not $ isLeftEmpty dce, not $ isRightEmpty dce) of
                   (False, False) -> "G"
                   (False, True ) -> "I"
                   (True , False) -> "D"
                   (True , True ) -> "A"


arbitraryOfSize :: Word -> Gen DynamicCharacterElement
arbitraryOfSize alphabetLen = do
    tag <- choose (0, 9) :: Gen Word
    let eGen   = fromNumber alphabetLen <$> choose (1 :: Integer, 2 ^ alphabetLen - 1)
    let delGen = deleteElement . AG <$> eGen
    let insGen = insertElement . AG <$> eGen
    case tag of
      0 -> pure . DCE $ fromNumber (2 * alphabetLen) (0 :: Integer)
      1 -> insGen
      2 -> insGen
      3 -> delGen
      4 -> delGen
      _ -> liftA2 alignElement (AG <$> eGen) (AG <$> eGen)


isLeftEmpty  :: DynamicCharacterElement -> Bool
isLeftEmpty  = isZeroVector . coerce . getLeft


isRightEmpty :: DynamicCharacterElement -> Bool
isRightEmpty = isZeroVector . coerce . getRight


{-# INLINE getLeft #-}
getLeft  :: DynamicCharacterElement -> AmbiguityGroup
getLeft  e@(DCE x) = AG $ subRange (0, symbolCount e - 1) x


{-# INLINE getRight #-}
getRight :: DynamicCharacterElement -> AmbiguityGroup
getRight e@(DCE x) = AG $ subRange (symbolCount e, dimension x - 1) x


{-# INLINE packDynamicCharacterElement #-}
packDynamicCharacterElement :: DynamicCharacterElement -> BitVector
packDynamicCharacterElement (DCE x) = x


{-# INLINE zeroVectorOf #-}
zeroVectorOf :: BitVector -> BitVector
zeroVectorOf = flip fromNumber (0 :: Word) . dimension
