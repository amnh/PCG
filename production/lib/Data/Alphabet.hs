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
-- An 'Alphabet' represents an ordered list of unique symbols with constant
-- time random access. Symbols are any data type which are coercable from a
-- 'String' through the 'IsString' type-class.
--
-- An 'Alphabet' is constructed by supplying a `Foldable` structure of symbols
-- which are 'IsString' instances to the 'constructAlphabet' function.
--
-- Every 'Alphabet' contains a "gap" symbol denoted by the @'fromString' "-"@
-- expression. The "gap" character is always the last element in the ordered
-- list regardless of it's presence or position in the construction structure.
--
-- An 'Alphabet' will never contain the "missing" symbol denoted by the
-- @'fromString' "?"@ expression. This symbol will be removed from the 'Alphabet'
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
import           Data.List                    (elemIndex, intercalate)
import           Data.Matrix.NotStupid        (Matrix, matrix)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                     (delete)
import           Data.Set              as Set (fromList)
import           Data.String
import           Data.Vector                  (Vector, generate)
import qualified Data.Vector           as V
import           Prelude               hiding (lookup, zip)
import           Test.Tasty.QuickCheck hiding (generate)
import           Test.QuickCheck.Arbitrary.Instances ()

{- PRECONDITION: We must insure that missing and gap are appropriately 
   code as "-" & "?", respectively, before this module is used, i.e., as output 
   from either parsers or in unification step.
 -}

-- | Newtyped to ensure invariants are preserved.
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

instance (Arbitrary a, Ord a, IsString a) => Arbitrary (Alphabet a) where
  arbitrary = constructAlphabet <$> listOf1 arbitrary

-- | Constructs an 'Alphabet' from a 'Foldable' structure of 'IsString' values.
--
--   /O(n*log(n))/
constructAlphabet :: (Ord a, IsString a, Foldable t) => t a -> Alphabet a
constructAlphabet = Alphabet . V.fromList . appendGapSymbol . toList . removeSpecialSymbols . Set.fromList . toList
  where
    gapSymbol            = fromString "-"
    missingSymbol        = fromString "?"
    appendGapSymbol      = (<> [gapSymbol])
    removeSpecialSymbols = delete missingSymbol . delete gapSymbol
    
-- | Retreives the "gap character" from the alphabet.
--
--   /O(1)/
gapCharacter :: Alphabet a -> a
gapCharacter alphabet = alphabet ! (length alphabet - 1)

-- | Constructs an 'Alphabet' with a corresponding TCM. Permutes TCM rows and
--   columns as the 'Alphabet' is reordered. Deletes TCM rows and columns where
--   'Alphabet' symbols are eliminated.
--
--   /O(n*log(n) + n^2)/
constructAlphabetWithTCM :: (Ord a, IsString a, Foldable t) => t a -> Matrix b -> (Alphabet a, Matrix b)
constructAlphabetWithTCM symbols originalTcm = (alphabet, permutedTcm)
  where
    alphabet    = constructAlphabet symbols
    len         = length alphabet
    oldOrdering = generate len (\x -> fromJust $ (alphabet ! x) `elemIndex` toList symbols)
    permutedTcm = matrix len len f
    f (i,j) =  originalTcm ! (oldOrdering V.! i, oldOrdering V.! j)
