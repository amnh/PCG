------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.SharedContinugousMetatdata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Bio.Sequence.SharedContinugousMetatdata
  ( SharedMetatdataIntervals()
  , GeneralCharacterMetadata(..)
  , (!)
  , singleton
  ) where


import           Bio.Metadata.CharacterName
import           Data.Alphabet
import           Data.Foldable
import           Data.Key                       (index)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.ReplicatedSequence        (ReplicatedSequence)
import qualified Data.ReplicatedSequence as Rep


data GeneralCharacterMetadata
   = GeneralCharacterMetadata
   { characterAlphabet :: Alphabet String
   , characterName     :: CharacterName
   , characterWeight   :: Double
   } deriving (Eq) 


newtype SharedMetatdataIntervals = SMI (ReplicatedSequence GeneralCharacterMetadata)


type instance Element SharedMetatdataIntervals = GeneralCharacterMetadata


instance Monoid SharedMetatdataIntervals where

  mempty = SMI mempty

  lhs `mappend` rhs = SMI $ unwrap lhs <> unwrap rhs


instance MonoFoldable SharedMetatdataIntervals where

  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  {-# INLINE ofoldMap #-}
  ofoldMap f = foldMap f . unwrap

  -- | Right-associative fold of a monomorphic container.
  ofoldr f e = foldr f e . unwrap

  -- | Strict left-associative fold of a monomorphic container.
  {-# INLINE ofoldl' #-}
  ofoldl' f e  = foldl' f e . unwrap

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldr1Ex #-}
  ofoldr1Ex f = foldr1 f . unwrap

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldl1Ex' #-}
  ofoldl1Ex' f = foldl1 f . unwrap

  {-# INLINE onull #-}
  onull = null . unwrap

  {-# INLINE olength #-}
  olength = length . unwrap


{-# INLINE singleton #-}
singleton :: Int -> GeneralCharacterMetadata -> SharedMetatdataIntervals
singleton i = SMI . Rep.singleton i


{-# INLINE unwrap #-}
unwrap :: SharedMetatdataIntervals -> ReplicatedSequence GeneralCharacterMetadata
unwrap (SMI xs) = xs


{-# INLINE (!) #-}
(!) :: SharedMetatdataIntervals -> Int -> GeneralCharacterMetadata
(!) xs i = unwrap xs `index` i
