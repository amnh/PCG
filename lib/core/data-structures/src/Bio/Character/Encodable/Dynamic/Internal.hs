-------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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

{-# LANGUAGE ApplicativeDo              #-}
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
--  , selectDC
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
import           Control.Lens                                   ((^.))
import           Control.Monad                                  (unless, when)
import           Control.Monad.Loops                            (whileM)
import           Control.Monad.ST
import           Data.Alphabet
import           Data.Binary
import           Data.BitMatrix
import           Data.BitVector.LittleEndian
import           Data.BitVector.LittleEndian.Instances          ()
import           Data.Bits
import           Data.Coerce
import           Data.Foldable
import           Data.Hashable
import qualified Data.IntMap                                    as IM
import           Data.Key
import qualified Data.List.NonEmpty                             as NE
import           Data.List.Utility                              (invariantTransformation, occurrences)
import           Data.MonoTraversable
import           Data.STRef
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Vector                                    as EV
import qualified Data.Vector.Mutable                            as MV
import           Data.Vector.NonEmpty                           (Vector)
import qualified Data.Vector.NonEmpty                           as V
import qualified Data.Vector.Unboxed.Mutable                    as MUV
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances            ()
import           Text.XML
import           TextShow                                       (TextShow(showb))


-- |
-- Represents an encoded dynamic character, consisting of one or more static
-- characters. 'DynamicCharacter's treat entire static characters as the
-- character states of the dynamic character. The dynamic character relies on
-- the encoding of the individual static characters to define the encoding of
-- the entire dynamic character.
data  DynamicCharacter
    = Missing {-# UNPACK #-} !Word
    | DC      {-# UNPACK #-} !(Vector (BitVector, BitVector, BitVector))
    deriving stock    (Eq, Generic, Ord, Show, Read)
    deriving anyclass (Binary, NFData)


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
         DC    bvs -> Right $ toList bvs


instance EncodedAmbiguityGroupContainer DynamicCharacter where

    {-# INLINE symbolCount #-}
    symbolCount (Missing n) = n
    symbolCount (DC bvs)    =
        let (x,_,_) = bvs ! 0
        in  dimension x


instance EncodableDynamicCharacter DynamicCharacter where

    constructDynamic = DC . force . V.fromNonEmpty . fmap (\(DCE x) -> x) . toNonEmpty

    destructDynamic = NE.nonEmpty . otoList

    -- |
    -- Strips the gap elements from the supplied character.
    --
    -- Remembers the locations of the gap characters that were deleted
    --
    -- If the character contains /only/ gaps, a missing character is returned.
    {-# INLINEABLE deleteGaps #-}
    deleteGaps c@Missing{} = (mempty, c)
    deleteGaps c@(DC    bvs)
      | null gaps   = (gaps,            c)
      | newLen == 0 = (gaps, toMissing  c)
      | otherwise   = (gaps, force $ DC newVector)
      where
        newVector = runST $ do
            j <- newSTRef 0
            let isGapAtJ = do
                  j' <- readSTRef j
                  pure $ j' < charLen && getMedian (c `indexStream` j') == gap

            let g = do
                  whileM isGapAtJ (modifySTRef j succ)
                  j' <- readSTRef j
                  modifySTRef j succ
                  pure $ bvs ! j'

            V.generateM newLen $ const g

        gapCount = fromEnum . getSum $ foldMap Sum gaps
        charLen  = length bvs
        newLen   = charLen - gapCount
        gapElem  = gapOfStream c
        gap      = getMedian gapElem

        gaps = IM.fromDistinctAscList $ reverse refs
        refs = runST $ do
            nonGaps <- newSTRef 0
            prevGap <- newSTRef False
            gapLen  <- newSTRef 0
            gapRefs <- newSTRef []

            let handleGapBefore op = do
                    gapBefore <- readSTRef prevGap
                    when gapBefore $ do
                      j <- readSTRef nonGaps
                      g <- readSTRef gapLen
                      modifySTRef gapRefs ( (j,g): )
                      op

            for_ [0 .. charLen - 1] $ \i ->
              if getMedian (c `indexStream` i)  == gap
              then modifySTRef gapLen succ *> writeSTRef prevGap True
              else do handleGapBefore $ do
                        writeSTRef  gapLen 0
                        writeSTRef prevGap False
                      modifySTRef nonGaps succ

            handleGapBefore $ pure ()
            readSTRef gapRefs

    -- |
    -- Adds gaps elements to the supplied character.
    insertGaps lGaps rGaps meds
      | null lGaps && null rGaps = meds -- No work needed
      | otherwise                = force . DC . coerce $ newVector
      where
        gap       = getMedian $ gapOfStream meds
        totalGaps = fromEnum . getSum . foldMap Sum
        gapVecLen = maybe 0 (succ . fst) . IM.lookupMax
        lGapCount = totalGaps lGaps
        rGapCount = totalGaps rGaps
        newLength = lGapCount + rGapCount + olength meds

        ins = splitElement $ insertElement gap gap
        del = splitElement $ deleteElement gap gap

        newVector = EV.create $ do
          mVec <- MV.unsafeNew newLength
          lVec <- MUV.replicate (gapVecLen lGaps) 0
          rVec <- MUV.replicate (gapVecLen rGaps) 0
          lGap <- newSTRef 0
          mPtr <- newSTRef 0
          rGap <- newSTRef 0

          -- Write out to the mutable vectors
          for_ (IM.toAscList lGaps) $ uncurry (MUV.unsafeWrite lVec)
          for_ (IM.toAscList rGaps) $ uncurry (MUV.unsafeWrite rVec)

          let align i = do
                m <- readSTRef mPtr
                let e = meds `indexStream` m
                let v = coerce e
                MV.unsafeWrite mVec i v
                modifySTRef mPtr succ
                when (isAlign e || isDelete e) $ do
                  modifySTRef lGap succ
                when (isAlign e || isInsert e) $ do
                  modifySTRef rGap succ

          let insertGapWith i e gapRef gapVec = do
                rg <- readSTRef gapRef
                v  <- if rg >= MUV.length gapVec then pure 0 else MUV.unsafeRead gapVec rg
                if   v == 0
                then pure False
                else do MV.unsafeWrite mVec i e
                        MUV.unsafeWrite gapVec rg $ v - 1
                        pure True

          for_ [0 .. newLength - 1] $ \i -> do
            written <- insertGapWith i ins lGap lVec
            unless written $ do
              written' <- insertGapWith i del rGap rVec
              unless written' $ align i

          pure mVec


instance EncodableStream DynamicCharacter where


    encodeStream alphabet = DC . force . V.fromNonEmpty . fmap f . toNonEmpty
      where
        f x = let v = packAmbiguityGroup $ encodeElement alphabet x
              in  (v,v,v)

    {-# INLINE gapOfStream #-}
    gapOfStream x =
        let w = symbolCount x
            v = bit . fromEnum $ pred w
            z = fromNumber w (0 :: Word)
        in  DCE (v,z,z)

    indexStream (DC v)    i = DCE $ v ! i
    indexStream Missing{} i = error $ "Tried to index an missing character with index " <> show i

    lookupStream Missing{} _ = Nothing
    lookupStream (DC v) i
      | 0 > i     = Nothing
      | otherwise = Just . DCE $ v ! i


instance ExportableElements DynamicCharacter where

{-  toExportableElements
      :: (Subcomponent (Element c) -> Subcomponent (Element c) -> Subcomponent (Element c))
      -> c
      -> Maybe ExportableCharacterElements
-}
    toExportableElements _ Missing{} = Nothing
    toExportableElements _t dc         = Just ExportableCharacterElements
        { exportedElementCountElements = toEnum $ olength dc
        , exportedElementWidthElements = symbolCount dc
        , exportedCharacterElements    = toNumber . getMedian <$> otoList dc
        }
      where
        toNumber = toUnsignedNumber . packAmbiguityGroup

    fromExportableElements riCharElems = {-# SCC fromExportableElements #-} DC . force $ V.fromNonEmpty bvs
      where
        bvs = {-# SCC bvs #-} f <$> NE.fromList inputElems
        fromValue  = fromNumber charWidth . (toEnum :: Int -> Word) . fromEnum
        charWidth  = reimportableElementWidthElements riCharElems
        inputElems = {-# SCC inputElems #-} reimportableCharacterElements    riCharElems --  :: ![(CUInt, CUInt, CUInt)]
        f (x,y,z)  =
            let x' = fromValue x
                y' = fromValue y
                z' = fromValue z
            in  (x', y', z')


instance ExportableBuffer DynamicCharacter where

    toExportableBuffer Missing {} = error "Attempted to 'Export' a missing dynamic character to foreign functions."
    toExportableBuffer dc@(DC v) = ExportableCharacterBuffer r c . bitVectorToBufferChunks r c . expandRows . fromRows $ (\(x,_,_) -> x) <$> v
      where
        r = toEnum $ length v
        c = symbolCount dc

    fromExportableBuffer ecs = DC . force . V.fromNonEmpty . NE.fromList . fmap (\v -> (v,v,v)) . otoList $ factorRows elemWidth newBitVec
      where
        newBitVec = bufferChunksToBitVector elemCount elemWidth $ exportedBufferChunks ecs
        elemCount = ecs ^. exportedElementCount
        elemWidth = ecs ^. exportedElementWidth


instance Hashable DynamicCharacter where

    hashWithSalt salt (Missing n) = salt `xor` fromEnum n
    hashWithSalt salt (DC      v) = salt `xor` hashWithSalt salt v


instance MonoFoldable DynamicCharacter where

    {-# INLINE ofoldMap #-}
    ofoldMap _ Missing{} = mempty
    ofoldMap f (DC c)    = foldMap (f . DCE) $ toList c

    {-# INLINE ofoldr #-}
    ofoldr _ e Missing{} = e
    ofoldr f e (DC c)    = foldr (f . DCE) e $ toList c

    {-# INLINE ofoldl' #-}
    ofoldl' _ e Missing{} = e
    ofoldl' f e (DC c)    = foldl' (\acc x -> f acc (DCE x)) e $ toList c

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an initial accumulator!"
    ofoldr1Ex f (DC c)    = DCE . ofoldr1Ex (\x y -> splitElement $ f (DCE x) (DCE y)) $ toList c

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' _ Missing{} = error "Trying to mono-morphically fold over an empty structure without supplying an initial accumulator!"
    ofoldl1Ex' f (DC c)    = DCE . ofoldl1Ex' (\x y -> splitElement $ f (DCE x) (DCE y)) $ toList c

    {-# INLINE onull #-}
    onull Missing{} = True
    onull _         = False

    {-# INLINE olength #-}
    olength Missing{} = 0
    olength (DC c)    = length c

    {-# INLINE headEx #-}
    headEx dc =
      case dc of
        (DC c) | (not . null) c -> DCE . headEx $ toList c
        _                       -> error $ "call to DynamicCharacter.headEx with: " <> show dc

    {-# INLINE lastEx #-}
    lastEx dc =
      case dc of
        (DC c) | (not . null) c -> DCE . lastEx $ toList c
        _                       -> error $ "call to DynamicCharacter.lastEx with: " <> show dc


instance MonoFunctor DynamicCharacter where

    omap _ dc@Missing{} = dc
    omap f dc@(DC      v) =
      let dces = splitElement . f . DCE <$> v
          bits (m,_,_) = finiteBitSize m
      in  case invariantTransformation bits v of
            Just _  -> DC dces
            Nothing -> error $ unlines
               [ "The mapping function over the Dynamic Character did not return *all* all elements of equal length."
               , show . occurrences $ bits <$> v
               , unlines $ foldMap (\x -> if x then "1" else "0") . toBits . (\(x,_,_) -> x) <$> toList v
               , show dc
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
        contentTuple (DCE (m,l,r)) = ("Character_states", show (f m, f l, f r)) -- the value of this character
        f = fmap (\x -> if x then '1' else '0') . toBits


--selectDC :: DynamicCharacterElement -> Word -> Maybe Word
--selectDC = coerce select


-- |
-- Produce a 'DynamicCharacter' generator where the elements of the character
-- have the specified width.
arbitraryDynamicCharacterOfWidth :: Word -> Gen DynamicCharacter
arbitraryDynamicCharacterOfWidth alphabetLen = do
    characterLen <- arbitrary `suchThat` (> 0) :: Gen Int
    let randVal   = arbitraryOfSize alphabetLen :: Gen DynamicCharacterElement
    bitRows      <- vectorOf characterLen randVal
    pure . DC . force . V.fromNonEmpty . NE.fromList . force $ splitElement <$> bitRows


-- |
-- Produce a rendering of the 'DynamicCharacter'.
--
-- Try to be intelligent about the rendering to improve user/debugging experience.
-- Render different alphabets in the most appealing manner, such as using IUPAC
-- codes for DNA/RNA.
renderDynamicCharacter
  :: Alphabet String
  -> (AmbiguityGroup -> AmbiguityGroup -> AmbiguityGroup)
  -> DynamicCharacter
  -> String
renderDynamicCharacter alphabet _transiton char
  | isMissing char = "<Missing>"
  | otherwise      =
    let shownElems = showStreamElement alphabet . getMedian <$> otoList char
    in  if   any (\e -> length e > 1) shownElems
        then unwords shownElems
        -- All elements were rendered as a single character.
        else fold shownElems
