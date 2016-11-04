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
-- An 'Alphabet' represents an /ordered/ list of unique symbols with constant
-- time random access. Symbols are any data type which are coercable from a
-- 'String' through the 'IsString' type-class.
--
-- An 'Alphabet' is constructed in one of two ways:
--
--  1. Supplying a `Foldable` structure of symbols which are 'IsString'
--     instances to the 'fromSymbols' function.
--
--  2. Supplying a `Foldable` structure of symbols and state name pairs,
--     both of which are 'IsString' instances to the 'fromSymbolsWithStateNames'
--     function.
--
-- Both 'Alphabet' construction methods are order preserving with respect to the
-- input symbol order.
--
-- Every 'Alphabet' contains a "gap" symbol denoted by the expression:
-- > fromString "-"
-- The "gap" character is always the last element in the ordered
-- list regardless of its presence or position in the construction structure.
--
-- An 'Alphabet' will never contain the "missing" symbol denoted by the
-- expression:
-- > fromString "?"
-- This symbol will be removed from the 'Alphabet'
-- if it is present in the construction structure.
-----------------------------------------------------------------------------   
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Alphabet
  ( Alphabet()
  , AmbiguityGroup
  , alphabetStateNames
  , alphabetSymbols
  , fromSymbols
  , fromSymbolsWithStateNames
  , isAlphabetDNA
  , isAlphabetAminoAcid
  , gapCharacter
  ) where

import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Key
import           Data.List                    (intercalate, sort)
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.String
import           Data.Vector                  (Vector)
import qualified Data.Vector           as V
import           Prelude               hiding (lookup, zip)
import           Test.Tasty.QuickCheck hiding (generate)
import           Test.QuickCheck.Arbitrary.Instances ()

{- PRECONDITION: We must ensure that missing and gap are appropriately 
   code as "-" & "?", respectively, before this module is used, i.e., as output 
   from either parsers or in unification step.
 -}

-- |
-- A non empty collection of symbols from an 'Alphabet'.
type AmbiguityGroup a = NonEmpty a


-- |
-- A collection of symbols and optional corresponding state names.
data Alphabet a 
   = SimpleAlphabet     (Vector (UnnamedSymbol a))
   | StateNamedAlphabet (Vector (  NamedSymbol a))

type instance Key Alphabet = Int

instance Indexable Alphabet where
  {-# INLINE index #-}
  index a i = fromMaybe raiseError $ i `lookup` a
    where
      raiseError = error $ mconcat
                 ["Error indexing Alphabet at location "
                 , show i
                 , ", valid inclusive index range is [0, "
                 , show $ length a - 1
                 , "]."
                 ]


instance Lookup Alphabet where
  {-# INLINE lookup #-}
  lookup i alphabet = symbolVector alphabet V.!? i


instance Foldable Alphabet where
  {-# INLINE foldr #-}
  foldr  f e = V.foldr  f e . symbolVector

  {-# INLINE foldl #-}
  foldl  f e = V.foldl  f e . symbolVector

  {-# INLINE foldr1 #-}
  foldr1 f   = V.foldr1 f   . symbolVector

  {-# INLINE foldl1 #-}
  foldl1 f   = V.foldl1 f   . symbolVector

  {-# INLINE length #-}
  length = V.length . symbolVector


instance FoldableWithKey Alphabet where
  {-# INLINE foldrWithKey #-}
  foldrWithKey f e = V.ifoldr' f e . symbolVector

  {-# INLINE foldlWithKey #-}
  foldlWithKey f e = V.ifoldl' f e . symbolVector


-- | /O(n * log n)/
instance Ord a => Eq (Alphabet a) where
  lhs == rhs =  length lhs == length rhs
             && sort (toList lhs) == sort (toList rhs)


instance Show a => Show (Alphabet a) where
  show x = mconcat [ "Alphabet: {"
                   , intercalate ", " $ show <$> toList x
                   , "}"
                   ]


instance (Ord a, IsString a) => Arbitrary (Alphabet a) where
    arbitrary = do
        n <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 62)
        pure . fromSymbols $ take n symbolSpace
      where
        -- We do this to simplify Aplahbet generation, ensuring that there is at least one non gap symbol.
        symbolSpace = fromString . pure <$> ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "?-"


-- | Constructs an 'Alphabet' from a 'Foldable' structure of symbols which are
--   'IsString' values.
--
--   /O(n * log n)/
fromSymbols :: (Ord a, IsString a, Foldable t) => t a -> Alphabet a
fromSymbols = SimpleAlphabet . fmap (Unnamed . toSingle) . alphabetPreprocessing . fmap fromSingle . toList


-- | Constructs an 'Alphabet' from a 'Foldable' structure of symbols and
--   coresponding state names, both of which a are 'IsString' values.
--
--   The input ordering is preserved.
--
--   /O(n * log n)/
fromSymbolsWithStateNames :: (Ord a, IsString a, Foldable t) => t (a,a) -> Alphabet a
fromSymbolsWithStateNames = StateNamedAlphabet . fmap (Named . toTuple) . alphabetPreprocessing . fmap fromTuple . toList


-- | Retreives the symbols of the 'Alphabet'. Synonym for 'toList'.
--
-- /O(n)/
alphabetSymbols :: Alphabet a -> [a]
alphabetSymbols = toList


-- | Retreives the state names for the symbols of the 'Alphabet'.
--
-- If there the symbols of the 'Alphabet' were not given state names during
-- construction then an empty list is returned.
--
-- /O(n)/
alphabetStateNames :: Alphabet a -> [a]
alphabetStateNames      SimpleAlphabet{}  = []
alphabetStateNames (StateNamedAlphabet v) = toList $ snd . fromNamed <$> v


-- | Retreives the "gap character" from the alphabet.
--
--   /O(1)/
gapCharacter :: Alphabet a -> a
gapCharacter alphabet = alphabet ! (length alphabet - 1)



{-
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 -   Supporting code and data structures:
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 -}



alphabetPreprocessing :: (Ord a, InternalClass a, Foldable t) => t a -> Vector a
alphabetPreprocessing = V.fromList . appendGapSymbol . removeSpecialSymbolsAndDuplicates . toList
  where
    appendGapSymbol       = (<> [gapSymbol])
    removeSpecialSymbolsAndDuplicates = (`evalState` mempty) . filterM f
      where
        f x
          | isGapSymboled     x = pure False
          | isMissingSymboled x = pure False
          | otherwise           = do
              seenSet <- get
              _       <- put $ x `Set.insert` seenSet
              pure $ x `notElem` seenSet

newtype UnnamedSymbol a = Unnamed  a
newtype NamedSymbol   a = Named (a,a)

fromUnnamed :: UnnamedSymbol t -> t
fromUnnamed (Unnamed x) = x

fromNamed   :: NamedSymbol t -> (t, t)
fromNamed   (Named   x) = x

symbolVector :: Alphabet b -> Vector b
symbolVector (SimpleAlphabet     v) =       fromUnnamed <$> v
symbolVector (StateNamedAlphabet v) = fst . fromNamed   <$> v

-- Newtypes for corecing and consolidation of alphabet input processing logic
newtype AlphabetInputSingle a = ASI  { toSingle ::  a    } deriving (Eq,Ord)
newtype AlphabetInputTuple  a = ASNI { toTuple  :: (a,a) } deriving (Eq,Ord)

fromSingle :: a -> AlphabetInputSingle a
fromSingle = ASI

fromTuple :: (a, a) -> AlphabetInputTuple a
fromTuple  = ASNI

class InternalClass a where
  gapSymbol         :: a
  isGapSymboled     :: a -> Bool
  isMissingSymboled :: a -> Bool

instance (Eq a, IsString a) => InternalClass (AlphabetInputSingle a) where
  gapSymbol         = ASI $ fromString "-"
  isGapSymboled     = (gapSymbol ==)
  isMissingSymboled = (ASI (fromString "?") ==)

instance (Eq a, IsString a) => InternalClass (AlphabetInputTuple a) where
  gapSymbol                      = ASNI (fromString "-", fromString "-")
  isGapSymboled     (ASNI (x,_)) = x == fromString "-"
  isMissingSymboled (ASNI (x,_)) = x == fromString "?"


{-
    
-- | Constructs an 'Alphabet' with a corresponding TCM. Permutes TCM rows and
--   columns as the 'Alphabet' is reordered. Deletes TCM rows and columns where
--   'Alphabet' symbols are eliminated.
--
--   If the alphabet has been permuted the coresponding TCM needs to be permuted in the same mannor.
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

-}


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetDNA :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetDNA = (`Set.isSubsetOf` dnaSymbolSet) . Set.fromList . toList
  where
    dnaSymbolSet = Set.fromList $ fromString . pure <$> "ACGTU-"


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents amino acid symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetAminoAcid :: (IsString s, Ord s) => Alphabet s -> Bool
isAlphabetAminoAcid = (`Set.isSubsetOf` aminoAcidSymbolSet) . Set.fromList . toList
  where
    aminoAcidSymbolSet = Set.fromList $ fromString . pure <$> "ACDEFGHIKLMNPQRSTVWY"
