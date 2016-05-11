-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An 'Alphabet represents an ordered list of unique symbols with constant
-- time random access. Symbols are any data type which are coercable from a
-- 'String' through the 'IsString' type-class.
--
-- An 'Alphabet is constructed by supplying a `Foldable` structure of symbols
-- which are 'IsString' instances to the 'constructAlphabet function.
--
-- Every 'Alphabet contains a "gap" symbol denoted by the 'fromString "-"'
-- expression. The "gap" character is always the last element in the ordered
-- list regardless of it's presence or position in the construction structure.
--
-- An 'Alphabet will never contain the "missing" symbol denoted by the
-- 'fromString "?"' expression. This symbol will be removed from the 'Alpahbet'
-- if it is present in the construction structure.
-----------------------------------------------------------------------------   
{-# LANGUAGE TypeFamilies #-}

module Data.Alphabet
  ( Alphabet(..)
  , constructAlphabet
  , constructAlphabetWithTCM
  , gapCharacter
  ) where

import           Data.Foldable
import           Data.Key
import           Data.List          (intercalate, nub)
import           Data.Matrix.NotStupid (Matrix, (<->), (<|>), getRow, rowVector, getCol, colVector)
import           Data.Monoid
import           Data.String
import           Data.Vector        (Vector)
import qualified Data.Vector as V
import           Prelude     hiding (lookup, zip)
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances ()

-- TODO: Alphabetize alphabets so that
--       'constructAlphabet "ACGT" == constructAlphabet "GATC === True'
--       This is okay as long as Additive characters have their additive
--       properties are captured in a TCM. Make sure that the additive character
--       TCMs are being generated properly in the rectification process.

-- Newtyped to ensure that there are no repeats.
{- | An 'Alphabet represents an ordered list of unique symbols with constant
     time random access. Symbols are any data type which are coercable from a
     'String' through the 'IsString' type-class.

     An 'Alphabet is constructed by supplying a `Foldable` structure of symbols
     which are 'IsString' instances to the 'constructAlphabet function.

     Every 'Alphabet contains a "gap" symbol denoted by the 'fromString "-"'
     expression. The "gap" character is always the last element in the ordered list
     regardless of it's presence or position in the construction structure.

     An 'Alphabet will never contain the "missing" symbol denoted by the
     'fromString "?"' expression. This symbol will be removed from the 'Alpahbet'
     if it is present in the construction structure. 
 -}

newtype Alphabet a
      = Alphabet (Vector a)
      deriving (Eq)

type instance Key Alphabet = Int

instance Indexable Alphabet where
  {-# INLINE index #-}
  index a i = case i `lookup` a of
                Just x  -> x
                Nothing -> error
                         $ mconcat ["Error indexing Alphabet at location "
                                   , show i
                                   , ", valid inclusive index range is [0, "
                                   , show $ length a - 1
                                   , "]."
                                   ]

instance Lookup Alphabet where
  {-# INLINE lookup #-}
  lookup i (Alphabet v) = v V.!? i


instance Foldable Alphabet where
  {-# INLINE foldr #-}
  foldr  f e (Alphabet v) = V.foldr f e v

  {-# INLINE foldl #-}
  foldl  f e (Alphabet v) = V.foldl f e v

  {-# INLINE foldr1 #-}
  foldr1 f (Alphabet v) = V.foldr1 f v

  {-# INLINE foldl1 #-}
  foldl1 f (Alphabet v) = V.foldl1 f v

  {-# INLINE length #-}
  length (Alphabet v) = V.length v


instance FoldableWithKey Alphabet where
  {-# INLINE foldrWithKey #-}
  foldrWithKey f e (Alphabet v) = V.ifoldr' f e v

  {-# INLINE foldlWithKey #-}
  foldlWithKey f e (Alphabet v) = V.ifoldl' f e v


instance Show a => Show (Alphabet a) where
  show (Alphabet v) = mconcat [ "Alphabet: {"
                               , intercalate ", " $ show <$> toList v
                               , "}"
                               ]

instance (Arbitrary a, Eq a, IsString a) => Arbitrary (Alphabet a) where
  arbitrary = constructAlphabet <$> arbitrary

-- TODO: Chagne constraint EQ a to Ord a and alphabetize Alphabet with sort
-- | Constructs an 'Alphabet from a 'Foldable structure of 'IsString' values.
constructAlphabet :: (Eq a, IsString a, Foldable t) => t a -> Alphabet a
constructAlphabet = Alphabet . V.fromList . appendGapSymbol . nub . removeSpecialSymbols . toList
  where
    appendGapSymbol      = (<> [gapSymbol])
    removeSpecialSymbols = filter (\x -> x /= gapSymbol
                                      && x /= missingSymbol)
    gapSymbol     = fromString "-"
    missingSymbol = fromString "?"

-- | Retreives the "gap character" from the alphabet.
gapCharacter :: Alphabet a -> a
gapCharacter alphabet = alphabet ! (length alphabet - 1)


-- | Constructs an 'Alphabet with a corresponding TCM. Permutes TCM rows and
--   columns as the 'Alphabet is reordered. Deletes TCM rows and columns where
--   'Alphabet symbols are eliminated.
constructAlphabetWithTCM :: (Eq a, IsString a, Foldable t) => t a -> Matrix b -> (Alphabet a, Matrix b)
constructAlphabetWithTCM inAlph inMat = (Alphabet $ V.fromList $ foldr (\v acc -> fst v : acc) mempty reordered, outCols)
  where
    gapSymbol     = fromString "-"
    missingSymbol = fromString "?"
    withPos = zip (toList inAlph) [0..]
    withGap = (<> [(gapSymbol, length withPos)])
    removeMissing = filter (\v -> fst v /= missingSymbol)
    fstIn e l = foldr (\v acc -> if (fst v) == (fst e) then True else acc) False l 
    removeDuplicates = foldr (\v acc -> if v `fstIn` acc then v : acc else acc) mempty
    reordered = removeDuplicates $ withGap $ removeMissing withPos
    toGrab = foldr (\v acc -> snd v : acc) mempty reordered
    outRows = foldl (\acc r -> acc <-> rowVector (getRow r inMat)) (rowVector $ getRow (head toGrab) inMat) (tail toGrab)
    outCols = foldl (\acc r -> acc <|> colVector (getCol r inMat)) (colVector $ getRow (head toGrab) inMat) (tail toGrab)
