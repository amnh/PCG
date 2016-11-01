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

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Sequence.SharedContinugousMetatdata
  ( SharedMetatdataIntervals()
  , DiscreteCharacterMetadata()
  , GeneralCharacterMetadata()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , (!)
    -- * Constructors
  , singleton
  , continuousMetadata
  , discreteMetadata
  ) where


import           Bio.Metadata.CharacterName
import           Control.Lens            hiding (index)
import           Data.Alphabet
import           Data.Foldable
import           Data.Key                       (index)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.ReplicatedSequence        (ReplicatedSequence)
import qualified Data.ReplicatedSequence as Rep


-- |
-- Represents a concrete type containing metadata fields shared across all
-- discrete different bins. Continous bins do not have Alphabets. 
data DiscreteCharacterMetadata
   = DiscreteCharacterMetadata
   { alphabet          :: Alphabet String
   , generalMetadata   :: GeneralCharacterMetadata
   } deriving (Eq, Show) 


-- |
-- Represents a concrete type containing metadata fields shared across different
-- bins.
data GeneralCharacterMetadata
   = GeneralCharacterMetadata
   { name     :: CharacterName
   , weight   :: Double
   } deriving (Eq, Show) 


-- |
-- A space efficient storage of repeating metadata fields.
--
-- /O(m)/ indexing, where /m/ is then number of /unique/ metadata values in the
-- structure.
--
-- Use 'singleton' and '(<>)' for construction.
newtype SharedMetatdataIntervals = SMI (ReplicatedSequence DiscreteCharacterMetadata)
  deriving (Eq, Show)


-- |
-- A 'Lens' for the 'characterAlphabet' field
class HasCharacterAlphabet s a | s -> a where

    characterAlphabet :: Lens' s a
    {-# MINIMAL characterAlphabet #-}


-- |
-- A 'Lens' for the 'characterName' field
class HasCharacterName s a | s -> a where

    characterName :: Lens' s a
    {-# MINIMAL characterName #-}


-- |
-- A 'Lens' for the 'characterWeight' field
class HasCharacterWeight s a | s -> a where

    characterWeight :: Lens' s a
    {-# MINIMAL characterWeight #-}


-- | (✔)
instance HasCharacterAlphabet DiscreteCharacterMetadata (Alphabet String) where

    characterAlphabet = lens alphabet $ \e x -> e { alphabet = x }


-- | (✔)
instance HasCharacterName DiscreteCharacterMetadata CharacterName where

    characterName = lens (name . generalMetadata)
                  $ \e x -> e { generalMetadata = (generalMetadata e) { name = x } }


-- | (✔)
instance HasCharacterWeight DiscreteCharacterMetadata Double where

    characterWeight = lens (weight . generalMetadata)
                    $ \e x -> e { generalMetadata = (generalMetadata e) { weight = x } }


-- | (✔)
instance HasCharacterName GeneralCharacterMetadata CharacterName where

    characterName = lens name $ \e x -> e { name = x }


-- | (✔)
instance HasCharacterWeight GeneralCharacterMetadata Double where

    characterWeight = lens weight $ \e x -> e { weight = x }


type instance Element SharedMetatdataIntervals = DiscreteCharacterMetadata


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


-- |
-- /O(1)/ construction.
--
-- @singleton n m@ creates a new 'SharedMetatdataIntervals' containing @n@
-- idenitcal values of @m@.
{-# INLINE singleton #-}
singleton :: Int -> DiscreteCharacterMetadata -> SharedMetatdataIntervals
singleton i = SMI . Rep.singleton i


-- |
-- /O(m)/ indexing, where /m/ is then number of /unique/ metadata values in the
-- structure. 
{-# INLINE (!) #-}
(!) :: SharedMetatdataIntervals -> Int -> DiscreteCharacterMetadata
(!) xs i = unwrap xs `index` i


continuousMetadata :: CharacterName -> Double -> GeneralCharacterMetadata
continuousMetadata = GeneralCharacterMetadata

discreteMetadata :: Alphabet String -> CharacterName -> Double -> DiscreteCharacterMetadata
discreteMetadata alphabetVal nameVal weightVal =
    DiscreteCharacterMetadata
    { alphabet        = alphabetVal
    , generalMetadata = continuousMetadata nameVal weightVal
    }


{-# INLINE unwrap #-}
unwrap :: SharedMetatdataIntervals -> ReplicatedSequence DiscreteCharacterMetadata
unwrap (SMI xs) = xs

