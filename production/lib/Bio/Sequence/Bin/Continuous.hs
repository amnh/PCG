------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Bin.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Bio.Sequence.Bin.Continuous
  ( ContinuousBin(..)
  , continuousBin
  ) where


import           Bio.Sequence.SharedContinugousMetatdata hiding (singleton)
import           Data.Foldable
import           Data.List.NonEmpty                      hiding (length,toList)
import           Data.Monoid                                    (mappend)
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.ReplicatedSequence                        (ReplicatedSequence)
import qualified Data.ReplicatedSequence                 as Rep
import           Data.Vector                                    (Vector)
import qualified Data.Vector                             as V


-- |
-- A bin of one or more real-valued characters and thier corresponding metadata.
--
-- Use 'continuousBin' and '(<>)' to construct larger bins with differing metadata.
data ContinuousBin c
   = ContinuousBin
   { characterStream :: Vector (Maybe c)
   , metatdataBounds :: ContinuousMetatdataIntervals
   } deriving (Eq,Show)


-- |
-- A space efficient storage of repeating metadata fields.
--
-- /O(m)/ indexing, where /m/ is then number of /unique/ metadata values in the
-- structure.
--
-- Use 'singleton' and '(<>)' for construction.
newtype ContinuousMetatdataIntervals = CMI (ReplicatedSequence GeneralCharacterMetadata)
  deriving (Eq, Show)


instance Semigroup (ContinuousBin c) where

  lhs <> rhs =
    ContinuousBin
      { characterStream = characterStream lhs `mappend` characterStream rhs
      , metatdataBounds = metatdataBounds lhs `mappend` metatdataBounds rhs
      }


type instance Element ContinuousMetatdataIntervals = GeneralCharacterMetadata


instance Monoid ContinuousMetatdataIntervals where

    mempty = CMI mempty

    lhs `mappend` rhs = CMI $ unwrap lhs <> unwrap rhs


instance MonoFoldable ContinuousMetatdataIntervals where

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


-- |
-- Constructs a non-empty bin of continuous character that all shared the same
-- metadata values.
continuousBin :: NonEmpty c -> GeneralCharacterMetadata -> ContinuousBin c
continuousBin continuousCharacters corespondingMetadata =
  ContinuousBin
    { characterStream = V.fromList . fmap Just $ toList continuousCharacters
    , metatdataBounds = singleton (length continuousCharacters) corespondingMetadata
    }


-- |
-- /O(1)/ construction.
--
-- @singleton n m@ creates a new 'SharedMetatdataIntervals' containing @n@
-- idenitcal values of @m@.
{-# INLINE singleton #-}
singleton :: Int -> GeneralCharacterMetadata -> ContinuousMetatdataIntervals
singleton i = CMI . Rep.singleton i


{-# INLINE unwrap #-}
unwrap :: ContinuousMetatdataIntervals -> ReplicatedSequence GeneralCharacterMetadata
unwrap (CMI xs) = xs
