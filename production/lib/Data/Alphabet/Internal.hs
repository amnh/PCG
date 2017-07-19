-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- We must ensure that missing and gap are appropriately
-- code as "-" & "?", respectively, before this module is used, i.e., as output
-- from either parsers or in unification step.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Alphabet.Internal
  ( Alphabet()
  , AmbiguityGroup
  , alphabetStateNames
  , alphabetSymbols
  , fromSymbols
  , fromSymbolsWithStateNames
  , gapSymbol
  , truncateAtSymbol
  , truncateAtMaxSymbol
  ) where

import           Control.DeepSeq              (NFData)
import           Control.Monad.State.Strict
import           Data.Bifunctor               (first)
import           Data.Foldable
import           Data.Key
import           Data.List                    (elemIndex, intercalate, sort)
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.String
import           Data.Vector                  (Vector)
import qualified Data.Vector           as V
import           GHC.Generics                 (Generic)
import           Prelude               hiding (lookup, zip)
import           Test.Tasty.QuickCheck hiding (generate)
import           Test.QuickCheck.Arbitrary.Instances ()
import           Text.XML.Custom


-- |
-- A non empty collection of symbols from an 'Alphabet'.
type AmbiguityGroup a = NonEmpty a


-- |
-- A collection of symbols and optional corresponding state names.
data Alphabet a =
     Alphabet
     { symbolVector      :: Vector a
     , stateNames        :: [a]
     } deriving (Generic)


type instance Key Alphabet = Int


-- Newtypes for corecing and consolidation of alphabet input processing logic
newtype AlphabetInputSingle a = ASI  { toSingle ::  a    } deriving (Eq,Ord)
newtype AlphabetInputTuple  a = ASNI { toTuple  :: (a,a) } deriving (Eq,Ord)


-- |
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-- -   Supporting code and data structures:
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
--
newtype UnnamedSymbol a = Unnamed  a  deriving (Generic)
newtype NamedSymbol   a = Named (a,a) deriving (Generic)


class InternalClass a where

  gapSymbol'        :: a
  isGapSymboled     :: a -> Bool
  isMissingSymboled :: a -> Bool


alphabetPreprocessing :: (Ord a, InternalClass a, Foldable t) => t a ->  [a]
alphabetPreprocessing = appendGapSymbol . removeSpecialSymbolsAndDuplicates . toList
  where
    appendGapSymbol       = (<> [gapSymbol'])
    removeSpecialSymbolsAndDuplicates = (`evalState` mempty) . filterM f
      where
        f x
          | isGapSymboled     x = pure False
          | isMissingSymboled x = pure False
          | otherwise           = do
              seenSet <- get
              _       <- put $ x `Set.insert` seenSet
              pure $ x `notElem` seenSet


-- |
-- /O(n)/
--
-- Retreives the state names for the symbols of the 'Alphabet'.
--
-- If there the symbols of the 'Alphabet' were not given state names during
-- construction then an empty list is returned.
alphabetStateNames :: Alphabet a -> [a]
alphabetStateNames = stateNames


-- |
-- /O(n)/
--
-- Retreives the symbols of the 'Alphabet'. Synonym for 'toList'.
alphabetSymbols :: Alphabet a -> [a]
alphabetSymbols = toList


fromSingle :: a -> AlphabetInputSingle a
fromSingle = ASI


-- |
-- /O(n * log n)/
--
-- Constructs an 'Alphabet' from a 'Foldable' structure of symbols which are
-- 'IsString' values.
fromSymbols :: (Ord a, IsString a, Foldable t) => t a -> Alphabet a
fromSymbols inputSymbols = Alphabet symbols []
  where
    symbols = V.fromList . fmap toSingle . alphabetPreprocessing . fmap fromSingle $ toList inputSymbols


-- |
-- /O(n * log n)/
--
-- Constructs an 'Alphabet' from a 'Foldable' structure of symbols and
-- coresponding state names, both of which a are 'IsString' values.
--
-- The input ordering is preserved.
fromSymbolsWithStateNames :: (Ord a, IsString a, Foldable t) => t (a,a) -> Alphabet a
fromSymbolsWithStateNames inputSymbols = Alphabet symbols names
  where
    (symbols, names) = first V.fromList . unzip . fmap toTuple . alphabetPreprocessing . fmap fromTuple $ toList inputSymbols


fromTuple :: (a, a) -> AlphabetInputTuple a
fromTuple  = ASNI


-- |
-- /O(1)/
--
-- Retreives the "gap character" from the alphabet.
gapSymbol :: Alphabet a -> a
gapSymbol alphabet = alphabet ! (length alphabet - 1)


-- |
-- /O(n*log(n)/
--
-- Attempts to find the symbol in the Alphabet.
-- If the symbol exists, returns an alphabet with all the symbols occuring
-- before the supplied symbol included and all symbols occuring after the
-- supplied symbol excluded. The gap character is preserved in the alphabet
-- regardless of the supplied symbol.
truncateAtSymbol :: (Ord a, IsString a) => a -> Alphabet a -> Alphabet a
truncateAtSymbol symbol alphabet =
    case elemIndex symbol $ toList alphabet of
      Nothing -> alphabet
      Just i  ->
        case alphabetStateNames alphabet of
          [] -> fromSymbols               . take (i + 1) $      alphabetSymbols alphabet
          xs -> fromSymbolsWithStateNames . take (i + 1) $ zip (alphabetSymbols alphabet) xs


-- |
-- /O(n*log(n)/
--
-- Attempts to find the maximum provided symbol in the Alphabet.
-- If the any of the provided symbols exists, returns an alphabet including all
-- the symbols occuring before the maximum provided symbol and all symbols
-- occuring after the maximum supplied symbol excluded. The gap character is
-- preserved in the alphabet regardless of the supplied symbol.
truncateAtMaxSymbol :: (Foldable t, Ord a, IsString a) => t a -> Alphabet a -> Alphabet a
truncateAtMaxSymbol symbols alphabet =
    case maxIndex of
      Nothing -> alphabet
      Just i  ->
        case alphabetStateNames alphabet of
          [] -> fromSymbols               . take (i + 1) $      alphabetSymbols alphabet
          xs -> fromSymbolsWithStateNames . take (i + 1) $ zip (alphabetSymbols alphabet) xs
  where
    maxIndex = foldlWithKey' f Nothing alphabet
    f e k v
      | v `notElem` symbols = e
      | otherwise =
        case e of
          Nothing -> Just k
          Just  i -> Just $ max k i


instance NFData a => NFData (UnnamedSymbol a)
instance NFData a => NFData (  NamedSymbol a)


instance (Ord a, IsString a) => Arbitrary (Alphabet a) where

    arbitrary = do
        n <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 62)
        pure . fromSymbols $ take n symbolSpace
      where
        -- We do this to simplify Alphabet generation, ensuring that there is at least one non gap symbol.
        symbolSpace = fromString . pure <$> ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "?-"


-- |
-- /O(n * log n)/
instance Ord a => Eq (Alphabet a) where

    lhs == rhs =  length lhs == length rhs
               && sort (toList lhs) == sort (toList rhs)


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


instance (Eq a, IsString a) => InternalClass (AlphabetInputSingle a) where

  gapSymbol'        = ASI $ fromString "-"
  isGapSymboled     = (gapSymbol' ==)
  isMissingSymboled = (ASI (fromString "?") ==)


instance (Eq a, IsString a) => InternalClass (AlphabetInputTuple a) where

  gapSymbol'                     = ASNI (fromString "-", fromString "-")
  isGapSymboled     (ASNI (x,_)) = x == fromString "-"
  isMissingSymboled (ASNI (x,_)) = x == fromString "?"


instance Lookup Alphabet where

    {-# INLINE lookup #-}
    lookup i alphabet = symbolVector alphabet V.!? i


instance NFData a => NFData (Alphabet a)


instance Show a => Show (Alphabet a) where

    show x = mconcat
        [ "Alphabet: {"
        , intercalate ", " $ show <$> toList x
        , "}"
        ]


-- | (✔)
instance (Show a) => ToXML (Alphabet a) where

    toXML alphabet = xmlElement "Alphabet" [] [("Symbols", Left $ show alphabet)]



{-

--fromUnnamed :: UnnamedSymbol t -> t
--fromUnnamed (Unnamed x) = x


--fromNamed   :: NamedSymbol t -> (t, t)
--fromNamed   (Named   x) = x

{-
symbolVector :: Alphabet b -> Vector b
symbolVector (SimpleAlphabet     v) =       fromUnnamed <$> v
symbolVector (StateNamedAlphabet v) = fst . fromNamed   <$> v
-}



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
