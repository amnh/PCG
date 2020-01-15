------------------------------------------------------------------------------
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

module Bio.Character.Encodable.Dynamic.Internal
  ( DynamicCharacter (DC, Missing)
  , DynamicCharacterElement()
  , arbitraryDynamicCharacterOfWidth
  , renderDynamicCharacter
  ) where

import           Bio.Character.Encodable.Dynamic.AmbiguityGroup
import           Bio.Character.Encodable.Dynamic.Class
import           Bio.Character.Encodable.Dynamic.Element
import           Bio.Character.Encodable.Internal
import           Bio.Character.Encodable.Stream
import           Bio.Character.Exportable
import           Control.DeepSeq
import           Control.Lens
import           Data.Alphabet
import           Data.BitMatrix
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.Foldable
import           Data.Hashable
import qualified Data.List.NonEmpty                    as NE
import           Data.List.Utility                     (invariantTransformation, occurrences)
import           Data.MonoTraversable
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances   ()
import           Text.XML
import           TextShow                              (TextShow (showb)) --, toString)

import Debug.Trace


-- |
-- Represents an encoded dynamic character, consisting of one or more static
-- characters. 'DynamicCharacter's treat entire static characters as the
-- character states of the dynamic character. The dynamic character relies on
-- the encoding of the individual static characters to define the encoding of
-- the entire dynamic character.
data  DynamicCharacter
    = Missing {-# UNPACK #-} !Word
    | DC      {-# UNPACK #-} !BitMatrix
    deriving stock    (Eq, Generic, Ord, Show)
    deriving anyclass (NFData)


type instance Element DynamicCharacter = DynamicCharacterElement


-- We restrict the 'DynamicCharacter' values generated to be non-empty.
-- Most algorithms assume a nonempty dynamic character.
instance Arbitrary DynamicCharacter where

    arbitrary = do
        alphabetLen <- arbitrary `suchThat` (\x -> 2 <= x && x <= 62) :: Gen Word
        arbitraryDynamicCharacterOfWidth alphabetLen


instance CoArbitrary DynamicCharacter where

    coarbitrary v = coarbitrary $
        case v of
         Missing w -> Left w
         DC     bm -> Right $ rows bm


instance EncodedAmbiguityGroupContainer DynamicCharacter where

    {-# INLINE symbolCount #-}
    symbolCount (Missing n) = n
    symbolCount (DC c)      = numCols c `div` 2


instance EncodableDynamicCharacter DynamicCharacter where

    constructDynamic = DC . fromRows . fmap packDynamicCharacterElement . toList

    destructDynamic = NE.nonEmpty . otoList


instance EncodableStream DynamicCharacter where

    encodeStream alphabet = DC . fromRows . fmap f . toList
      where
        f x = let !v = packAmbiguityGroup $ (encodeElement alphabet x :: AmbiguityGroup)
              in  v <> v

    {-# INLINE gapOfStream #-}
    gapOfStream = bit . fromEnum . pred . symbolCount

    lookupStream (Missing{}) _ = Nothing
    lookupStream (DC bm) i
      | 0 > i     = Nothing
      | otherwise = let !j = toEnum i
                    in  if j < numRows bm
                        then Just . DCE $ bm `row` j
                        else Nothing


instance ExportableElements DynamicCharacter where

{-  toExportableElements
      :: (Subcomponent (Element c) -> Subcomponent (Element c) -> Subcomponent (Element c))
      -> c
      -> Maybe ExportableCharacterElements
-}
    toExportableElements _ Missing{} = Nothing
    toExportableElements t dc        = Just ExportableCharacterElements
        { exportedElementCountElements = toEnum $ olength dc
        , exportedElementWidthElements = symbolCount dc
        , exportedCharacterElements    = toNumber . getMedian t <$> otoList dc
        }
      where
        toNumber = toUnsignedNumber . packAmbiguityGroup
    
    fromExportableElements riCharElems = DC $ fromRows bvs
      where
        bvs = packDynamicCharacterElement . f <$> inputElems
        fromValue  = AG . fromNumber charWidth
        charWidth  = reimportableElementWidthElements riCharElems
        inputElems = reimportableCharacterElements    riCharElems --  :: ![(CUInt, CUInt, CUInt)]
        gap = bit . fromEnum $ charWidth - 1
        f (x,y,z) =
            let x' = fromValue x
                y' = fromValue y
                z' = fromValue z
            in  case (y' == gap, z' == gap) of
                  (False, False)             ->    gapElement charWidth
--                  (False, True ) | x' == y'  ->  alignElement y' z'
                  (False, True ) | otherwise -> insertElement    z'
--                  (True , False) | x' == z'  ->  alignElement y' z'
                  (True , False) | otherwise -> deleteElement y'
                  (True , True )             ->  alignElement y' z'


instance ExportableBuffer DynamicCharacter where

    toExportableBuffer Missing {} = error "Attempted to 'Export' a missing dynamic character to foreign functions."
    toExportableBuffer (DC bm) = ExportableCharacterBuffer x y . bitVectorToBufferChunks x y $ expandRows bm
      where
        x = numRows bm
        y = numCols bm

    fromExportableBuffer ecs = DC $ factorRows elemWidth newBitVec
      where
        newBitVec = bufferChunksToBitVector elemCount elemWidth $ exportedBufferChunks ecs
        elemCount = ecs ^. exportedElementCount
        elemWidth = ecs ^. exportedElementWidth


instance Hashable DynamicCharacter where

    hashWithSalt salt (Missing n) = salt `xor` fromEnum n
    hashWithSalt salt (DC     bm) = salt `xor` fromEnum (numCols bm) `xor` hashWithSalt salt bm


instance MonoFoldable DynamicCharacter where

    {-# INLINE ofoldMap #-}
    ofoldMap _ Missing{} = mempty
    ofoldMap f (DC c)    = ofoldMap (f . DCE) c

    {-# INLINE ofoldr #-}
    ofoldr _ e Missing{} = e
    ofoldr f e (DC c)    = ofoldr (f . DCE) e c

    {-# INLINE ofoldl' #-}
    ofoldl' _ e Missing{} = e
    ofoldl' f e (DC c)    = ofoldl' (\acc x -> f acc (DCE x)) e c

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an initial accumulator!"
    ofoldr1Ex f (DC c)    = DCE . ofoldr1Ex (\x y -> packDynamicCharacterElement $ f (DCE x) (DCE y)) $ c

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an initial accumulator!"
    ofoldl1Ex' f (DC c)    = DCE . ofoldl1Ex' (\x y -> packDynamicCharacterElement $ f (DCE x) (DCE y)) $ c

    {-# INLINE onull #-}
    onull Missing{} = True
    onull _         = False

    {-# INLINE olength #-}
    olength Missing{} = 0
    olength (DC c)    = olength c

    {-# INLINE headEx #-}
    headEx dc =
      case dc of
        (DC c) | (not . onull) c -> DCE $ headEx c
        _                        -> error $ "call to DynamicCharacter.headEx with: " <> show dc

    {-# INLINE lastEx #-}
    lastEx dc =
      case dc of
        (DC c) | (not . onull) c -> DCE $ lastEx c
        _                        -> error $ "call to DynamicCharacter.lastEx with: " <> show dc


instance MonoFunctor DynamicCharacter where

    omap f bm =
        case f <$> otoList bm of
          []   -> bm
          dces -> case invariantTransformation finiteBitSize dces of
             Just i  -> DC . factorRows (toEnum i) $ foldMap packDynamicCharacterElement dces
             Nothing -> error $ unlines
                 [ "The mapping function over the Dynamic Character did not return *all* all elements of equal length."
                 , show . occurrences $ finiteBitSize <$> dces
                 , show $ finiteBitSize <$> dces
                 , unlines $ foldMap (\x -> if x then "1" else "0") . toBits . packDynamicCharacterElement <$> dces
                 , show bm
                 ]


instance PossiblyMissingCharacter DynamicCharacter where

    {-# INLINE toMissing  #-}
    toMissing c = Missing $ symbolCount c

    {-# INLINE isMissing  #-}
    isMissing Missing{} = True
    isMissing _         = False


instance TextShow DynamicCharacter where

    showb (Missing w)  = "Missing " <> showb w
    showb (DC      bm) = "DC "      <> showb bm


instance ToXML DynamicCharacter where

    toXML dynamicChar = xmlElement "Dynamic_character" attributes contents
      where
        attributes            = []
        contents              = Left . contentTuple <$> otoList dynamicChar -- toXML on all dynamic character elements
        contentTuple (DCE bv) = ("Character_states", (\x -> if x then '1' else '0') <$> toBits bv) -- the value of this character


arbitraryDynamicCharacterOfWidth :: Word -> Gen DynamicCharacter
arbitraryDynamicCharacterOfWidth alphabetLen = do
    characterLen  <- arbitrary `suchThat` (> 0) :: Gen Int
    let randVal    = arbitraryOfSize alphabetLen :: Gen DynamicCharacterElement
    bitRows       <- vectorOf characterLen randVal
    pure . DC . fromRows . force $ packDynamicCharacterElement <$> bitRows


renderDynamicCharacter
  :: Alphabet String
  -> (AmbiguityGroup -> AmbiguityGroup -> AmbiguityGroup)
  -> DynamicCharacter
  -> String
renderDynamicCharacter alphabet transiton char
  | isMissing char = "<Missing>"
  | otherwise      =
    let shownElems = showStreamElement alphabet . getMedian transiton <$> otoList char
    in  if   any (\e -> length e > 1) shownElems
        then unwords shownElems
        -- All elements were rendered as a single character.                                          
        else fold shownElems
