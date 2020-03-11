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
{-# LANGUAGE StrictData                 #-}
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
--  , packDynamicCharacterElement
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
import           Data.MetricRepresentation
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances   ()
import           TextShow                              (TextShow (showb), toString)


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
      = DCE { splitElement :: (BitVector, BitVector, BitVector) }
      deriving stock   (Generic)
      deriving newtype (Eq, Hashable, NFData, Ord)


type instance Bound DynamicCharacterElement = Word


type instance Subcomponent DynamicCharacterElement = AmbiguityGroup


instance Arbitrary DynamicCharacterElement where

    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Int
        arbitraryOfSize $ toEnum alphabetLen


instance CoArbitrary DynamicCharacterElement


instance EncodedAmbiguityGroupContainer DynamicCharacterElement where

    {-# INLINE symbolCount  #-}
    symbolCount (DCE ~(m,_,_)) = dimension m


instance EncodableDynamicCharacterElement DynamicCharacterElement where
  
    isGap    (DCE ~(m,l,r)) =      isZeroVector l  &&      isZeroVector r

    isInsert (DCE ~(m,l,r)) =      isZeroVector l  && not (isZeroVector r)

    isDelete (DCE ~(m,l,r)) = not (isZeroVector l) &&     (isZeroVector r)

    isAlign  (DCE ~(m,l,r)) = not (isZeroVector l) && not (isZeroVector r)
    
    gapElement w                = let !z = fromNumber w (0 :: Word)
                                  in  DCE (bit . fromEnum $ w - 1, z, z)

    deleteElement (AG m)        (AG y) = DCE $ (m, zeroVectorOf y, y)

    insertElement (AG m) (AG x)        = DCE $ (m, x, zeroVectorOf x)

    alignElement  (AG m) (AG x) (AG y) = DCE $ (m, x, y)

    getContext dce =
        case (not $ isLeftEmpty dce, not $ isRightEmpty dce) of
          (False, False) -> Gapping
          (False, True ) -> Deletion
          (True , False) -> Insertion
          (True , True ) -> Alignment

    swapContext (DCE (m,l,r)) = DCE (m,r,l)

    getMedian (DCE (m,_,_)) = AG m


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

    showb dce@(DCE ~(m,l,r)) = fold
        [ "["
        , prefix
        , "|"
        , renderBits m
        , "|"
        , renderBits l
        , "|"
        , renderBits r
        , "]"
        ]
      where
        renderBits = fromString . ofoldMap g
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
    let gapped = gapElement alphabetLen
    let gap    = getMedian gapped 
    let eGen   = fromNumber alphabetLen <$> choose (1 :: Integer, 2 ^ alphabetLen - 1)
    let buildElem f v   = let v'  = AG v
                              med = fst $ discreteMetricPairwiseLogic gap v'
                          in  f med v'
    let unionElem f x y = let x'  = AG x
                              y'  = AG y
                              med = fst $ discreteMetricPairwiseLogic x' y'
                          in  f med x' y'    
    let delGen =         buildElem deleteElement <$> eGen
    let insGen =         buildElem insertElement <$> eGen
    let alnGen = liftA2 (unionElem alignElement) eGen eGen
    case tag of
      0 -> pure gapped
      1 -> insGen
      2 -> insGen
      3 -> delGen
      4 -> delGen
      _ -> alnGen


isLeftEmpty  :: DynamicCharacterElement -> Bool
isLeftEmpty  (DCE (_,l,_)) = isZeroVector l


isRightEmpty :: DynamicCharacterElement -> Bool
isRightEmpty (DCE (_,_,r)) = isZeroVector r


{-# INLINE getLeft #-}
getLeft  :: DynamicCharacterElement -> AmbiguityGroup
getLeft  (DCE (_,l,_)) = AG l


{-# INLINE getRight #-}
getRight :: DynamicCharacterElement -> AmbiguityGroup
getRight (DCE (_,_,r)) = AG r


{-
{-# INLINE packDynamicCharacterElement #-}
packDynamicCharacterElement :: DynamicCharacterElement -> BitVector
packDynamicCharacterElement (DCE x) = x
-}


{-# INLINE zeroVectorOf #-}
zeroVectorOf :: BitVector -> BitVector
zeroVectorOf = flip fromNumber (0 :: Word) . dimension
