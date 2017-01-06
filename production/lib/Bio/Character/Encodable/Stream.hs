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

module Bio.Character.Encodable.Stream where

import           Bio.Character.Encodable.Internal
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import           Data.BitVector     hiding (concat)
import           Data.Foldable
import           Data.List                 (intercalate)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility 
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.String               (IsString)


{-# DEPRECATED getGapChar "Don't use getGapChar, use getGapElement instead!" #-}


-- TODO: Add more laws here.
-- TODO: Remove Num constraint?
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
      , Num b -- Required for bit twiddling hacks
      ) => EncodableStreamElement b where

    decodeElement :: Eq a => Alphabet a -> b -> AmbiguityGroup a

    encodeElement :: (Eq a, IsString a) => Alphabet a -> AmbiguityGroup a -> b

    {-# INLINE getGapElement #-}
    getGapElement :: b -> b
    getGapElement = bit . pred . symbolCount

    getGapChar    :: b -> b
    getGapChar    = getGapElement


-- TODO: Add more laws here
{- |
 Represents a non empty stream of 'EncodableStreamElement' of variable length.

 Laws:

   * @decodeMany alphabet . encodeMany alphabet == fmap toList . toList@

-}
class ( EncodableStreamElement (Element s)
      , MonoTraversable s
      ) => EncodableStream s where
  
    decodeStream :: (Ord a, IsString a) => Alphabet a -> s -> NonEmpty (AmbiguityGroup a)
    decodeStream alphabet = NE.fromList . ofoldMap (\e -> [decodeElement alphabet e])

    encodeStream :: (Ord a, IsString a) => Alphabet a -> NonEmpty (AmbiguityGroup a) -> s

    indexStream  :: s -> Int -> Element s
    indexStream xs i = fromMaybe raiseError $ xs `lookupStream` i
      where
        raiseError = error
                   $ mconcat ["Index ", show i, " is out of range [0,", show $ olength xs - 1,"]."] 

    lookupStream :: s -> Int -> Maybe (Element s)
    lookupStream xs i = fst $ ofoldl' f (Nothing, 0) xs
      where
        f (Nothing, n) e = if n == i then (Just e, n) else (Nothing, n + 1)
        f acc          _ = acc

    -- Should probably be overwritten for safety & efficiency.
    gapOfStream :: s -> Element s
    gapOfStream = getGapElement . headEx


-- | Show an 'EncodableStreamElement' by decoding it with it's corresponding alphabet.
showStreamElement :: EncodableStreamElement e => Alphabet String -> e -> String
showStreamElement alphabet element = renderAmbiguity $ toIUPAC symbols
  where
    symbols   = decodeElement alphabet element
    renderAmbiguity amb =
        case toList amb of
          []  -> undefined -- Never occurs!
          [x] -> x
          xs  ->
              case invariantTransformation length xs of
                Just 1 -> "[" <> concat xs <> "]"
                _      -> "{" <> intercalate ", " xs <> "}"

    toIUPAC x
      | isAlphabetDna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToDna
      | isAlphabetRna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToRna
      | isAlphabetAminoAcid alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToAminoAcid
      | otherwise                    = x
