-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ReplicatedSequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor, TypeFamilies #-}

module Data.ReplicatedSequence
  ( ReplicatedSequence()
  , replicate
  , singleton
  ) where


import           Data.Foldable
import           Data.Key
import           Data.Maybe             (fromMaybe)
import           Data.Monoid     hiding ((<>))     
import           Data.Semigroup
import           Prelude         hiding (lookup,replicate)
import qualified Prelude         as P   (replicate)
import           Test.QuickCheck


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype ReplicatedSequence a = RSeq [(Int,a)]
   deriving (Eq,Functor,Ord)


-- |
-- Generation biases towards medium length
instance Arbitrary a => Arbitrary (ReplicatedSequence a) where

    arbitrary = RSeq <$> f 5
      where
        f n = do
          b <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= max n 1)
          if b == 0
          then pure []
          else do
            i <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 12)
            v <-  arbitrary
            ((i,v):) <$> f (n-1)


instance Foldable ReplicatedSequence where

    {-# INLINE fold #-}
    fold      = fold . expand

    {-# INLINE foldMap #-}
    foldMap f = foldMap f . expand

    {-# INLINE foldr #-}
    foldr f e = foldr f e . expand

    {-# INLINE toList #-}
    toList    = expand

    {-# INLINE null #-}
    null      = null . unwrap

    {-# INLINE length #-}
    length    = sum . fmap fst . unwrap
    
    {-# INLINE elem #-}
    elem e    = elem e . uniqueElems
  
    {-# INLINE maximum #-}
    maximum   = maximum . uniqueElems
  
    {-# INLINE minimum #-}
    minimum   = minimum . uniqueElems
  

type instance Key ReplicatedSequence = Int


instance Indexable ReplicatedSequence where

    {-# INLINE index #-}
    index a i = fromMaybe raiseError $ i `lookup` a
      where
        raiseError = error $ mconcat
          ["Error indexing ReplicatedSequence at location "
          , show i
          , ", valid inclusive index range is [0, "
          , show $ length a - 1
          , "]."
          ]


instance Lookup ReplicatedSequence where

    {-# INLINE lookup #-}
    lookup i = f i . unwrap
      where
        f _    []  = Nothing
        f n ((j,a):xs)
          | n < j     = Just a
          | otherwise = f (n - j) xs


instance Semigroup (ReplicatedSequence a) where

    {-# INLINE (<>) #-}
    lhs <> rhs = RSeq $ unwrap lhs <> unwrap rhs


instance Monoid (ReplicatedSequence a) where

    {-# INLINE mempty  #-}
    mempty = RSeq []

    {-# INLINE mappend #-}
    mappend = (<>)


instance Show a => Show (ReplicatedSequence a) where

    show = show . expand


-- |
-- /O(1)/
--
-- @replicate n x@ is a 'ReplicatedSequence' of the supplied element value repeated n times.
{-# INLINE replicate #-}
replicate :: Int -> a -> ReplicatedSequence a
replicate i e
  | i < 1     = error $ "Call to replicate with a non-positve value: " <> show i
  | otherwise = RSeq [(i,e)]


-- |
-- /O(1)/
--
-- A synomym for 'replicate'.
{-# INLINE singleton #-}
singleton :: Int -> a -> ReplicatedSequence a
singleton i e
  | i < 1     = error $ "Call to singleton with a non-positve value: " <> show i
  | otherwise = RSeq [(i,e)]


{-# INLINE expand #-}
expand :: ReplicatedSequence a -> [a]
expand =  foldMap (uncurry P.replicate) . unwrap


{-# INLINE uniqueElems #-}
uniqueElems :: ReplicatedSequence a -> [a]
uniqueElems = fmap snd . unwrap


{-# INLINE unwrap #-}
unwrap :: ReplicatedSequence a -> [(Int,a)]
unwrap (RSeq xs) = xs
