-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Stream
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Bio.Character.Encodable.Stream
  ( EncodableStreamElement(..)
  , EncodableStream(..)
  , showStreamElement
  , showStream
  , bitsInLocalWord
  , encodableStreamToExportableCharacterElements
  ) where

import           Bio.Character.Encodable.Internal
import           Bio.Character.Exportable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import           Data.Bits
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
import           Data.Maybe                (fromMaybe)
import           Data.MonoTraversable
import           Data.String               (IsString)
import           Foreign.C.Types


{-# DEPRECATED getGapChar "Don't use getGapChar, use getGapElement instead!" #-}


{- |
 Represents a character of fixed width encoding one or more character states.

 Laws:

   * @decodeElement alphabet . encodeChar alphabet . toList == id@

   * @encodeElement alphabet [alphabet ! i] == bit i@

   * @encodeElement alphabet alphabet == complement (bit (length alphabet - 1) `clearBit` (bit (length alphabet - 1))@

   * @decodeElement alphabet (encodeElement alphabet xs .|. encodeElement alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)@

   * @decodeElement alphabet (encodeElement alphabet xs .&. encodeElement alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)@

-}
class ( FiniteBits b
      , EncodedAmbiguityGroupContainer b
--      , Num b -- Required for bit twiddling hacks
      ) => EncodableStreamElement b where

    decodeElement :: Eq a => Alphabet a -> b -> AmbiguityGroup a

    encodeElement :: (Eq a, IsString a) => Alphabet a -> AmbiguityGroup a -> b

    {-# INLINE getGapElement #-}
    getGapElement :: b -> b
    getGapElement = bit . fromEnum . pred . symbolCount

    getGapChar    :: b -> b
    getGapChar    = getGapElement


{- |
 Represents a non-empty stream of 'EncodableStreamElement's of variable lengths.

 Laws:

   * @decodeMany alphabet . encodeMany alphabet == fmap toList . toList@

-}
class ( EncodableStreamElement (Element s)
      , MonoFoldable s
      , MonoFunctor  s
      ) => EncodableStream s where

    decodeStream :: (Ord a, IsString a) => Alphabet a -> s -> NonEmpty (AmbiguityGroup a)
    decodeStream alphabet = NE.fromList . ofoldMap (\e -> [decodeElement alphabet e])

    encodeStream :: (Ord a, IsString a) => Alphabet a -> NonEmpty (AmbiguityGroup a) -> s

    indexStream  :: s -> Int -> Element s
    indexStream xs i = fromMaybe raiseError $ xs `lookupStream` i
      where
        raiseError = error
                   $ mconcat ["Stream indexing at ", show i, " is out of range [0,", show $ olength xs - 1,"]."]

    lookupStream :: s -> Int -> Maybe (Element s)
    lookupStream xs i = fst $ ofoldl' f (Nothing, 0) xs
      where
        f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
        f acc          _ = acc

    -- Should probably be overwritten for safety & efficiency.
    gapOfStream :: s -> Element s
    gapOfStream = getGapElement . headEx


-- |
-- Show an 'EncodableStreamElement' by decoding it with its corresponding alphabet.
showStreamElement :: EncodableStreamElement e => Alphabet String -> e -> String
showStreamElement alphabet element
  | zeroBits == element = "<Empty Character>"
  |  allBits == element = "?"
  | otherwise           = renderAmbiguity $ toIUPAC symbols
  where
    allBits = complement $ element `xor` element
    symbols = decodeElement alphabet element
    renderAmbiguity amb =
        case toList amb of
          []  -> undefined -- Never occurs!
          [x] -> x
          xs  ->
              case invariantTransformation length xs of
                Just 1 -> "[" <> concat  xs <> "]"
                _      -> "[" <> unwords xs <> "]"

    toIUPAC x
      | isAlphabetDna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToDna
      | isAlphabetRna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToRna
      | isAlphabetAminoAcid alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToAminoAcid
      | otherwise                    = x


{-
-- |
-- Serialize any instance of 'FiniteBits' as a stream of 1s and 0s with the
-- left-hand side of the stream representing the least significant bit and the
-- right-hand side of the stream representing the most significant bit.
showBits :: FiniteBits b => b -> String
showBits b = foldMap f [0 .. finiteBitSize b - 1]
  where
    f i
      | b `testBit`  i = "1"
      | otherwise      = "0"
-}


-- |
-- Show an 'EncodableStream' by decoding it with its corresponding alphabet.
showStream :: EncodableStream s => Alphabet String -> s -> String
showStream alphabet = ofoldMap (showStreamElement alphabet)


-- |
-- Number of bits in a `Word` or `Int` type on this machine, derived at compile time.
bitsInLocalWord :: Word
bitsInLocalWord  = toEnum $ finiteBitSize (undefined :: CULong)


-- |
-- Convert an encobale stream to a concrete 'ExportableCharacterElements' value.
encodableStreamToExportableCharacterElements :: (EncodableStream s, EncodedAmbiguityGroupContainer s, Enum (Element s))
                                             => s -> Maybe ExportableCharacterElements
encodableStreamToExportableCharacterElements dc
  | bitsInElement > bitsInLocalWord = Nothing
  | otherwise                       = Just $ ExportableCharacterElements numberOfElements bitsInElement integralElements
  where
    bitsInElement    = symbolCount dc
    numberOfElements = toEnum $ olength dc
    integralElements = ofoldMap (pure . toEnum . fromEnum) dc
