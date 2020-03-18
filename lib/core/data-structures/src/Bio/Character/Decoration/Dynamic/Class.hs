-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- For derived instance of PossiblyMissingCharacter
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Character.Decoration.Dynamic.Class
  ( AverageLength()
  , DirectOptimizationDecoration
  , DirectOptimizationPostorderDecoration
  , DynamicCharacterDecoration(..)
  , ImpliedAlignmentDecoration
  , PostorderExtensionDirectOptimizationDecoration(..)
  , SimpleDynamicDecoration
  , SimpleDynamicExtensionPostorderDecoration(..)
  , HasAlignmentContext(..)
  , HasAverageLength(..)
  , HasCharacterLocalCost(..)
  , HasEncoded(..)
  , HasImpliedAlignment(..)
  , HasSingleDisambiguation(..)
  , getAverageLength
  , toAverageLength
  ) where


import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Character.Exportable
import Control.DeepSeq
import Control.Lens
import Data.MonoTraversable            (Element)
import GHC.Generics
import Numeric.NonNegativeAverage


-- |
-- The average length of all the sequences in the subtree.
--
-- Forms a 'Semigroup' for efficient recursive post-order accumulation on the tree.
newtype AverageLength = AL NonNegativeAverage
  deriving stock   (Eq, Generic, Ord)
  deriving newtype (Semigroup)


instance NFData AverageLength


instance Show AverageLength where

    show (AL x) = show x


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( HasAverageLength           s AverageLength
      , HasEncoded                 s a
      , EncodableDynamicCharacter  a
      , ExportableBuffer           (Subcomponent (Element a))
      ) => SimpleDynamicDecoration s a | s -> a where


-- |
-- An incomplete decoration of a dynamic character with half the direct optimization annotations.
--
-- Represents the result of just the post-order traversal.
--
-- Is a sub-class of 'DynamicCharacterDecoration'.
class ( HasCharacterCost        s Word
      , HasCharacterLocalCost   s Word
      , HasAlignmentContext     s a
      , SimpleDynamicDecoration s a
      , ExportableBuffer (Subcomponent (Element a))
--      , GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec (Subcomponent (Element a))) MemoizedCostMatrix
      ) => DirectOptimizationPostorderDecoration s a | s -> a where


-- |
-- A decoration of a dynamic character with all direct optimization annotations.
--
-- Is a sub-class of 'DirectOptimizationPostorderDecoration'.
class ( HasImpliedAlignment     s a
      , DirectOptimizationPostorderDecoration s a
      ) => DirectOptimizationDecoration s a | s -> a where


-- |
-- A decoration of a dynamic character with the implied alignment decorations.
--
-- Is a sub-class of 'DirectOptimizationDecoration'.
class ( HasImpliedAlignment           s a
      , DirectOptimizationDecoration  s a
      ) => ImpliedAlignmentDecoration s a | s -> a where


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( SimpleDynamicDecoration s a
      ) => DynamicCharacterDecoration s a | s -> a where

    toDynamicCharacterDecoration :: (x -> a) -> x -> s
    {-# MINIMAL toDynamicCharacterDecoration #-}


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( SimpleDynamicDecoration s c
      , DirectOptimizationPostorderDecoration s c
      ) => SimpleDynamicExtensionPostorderDecoration s c | s -> c where

    extendDynamicToPostorder :: SimpleDynamicDecoration x c
                             => x             -- ^ Original decoration
                             -> Word          -- ^ The cost of the alignment
                             -> Word          -- ^ The cost of the alignment and the child subtrees
                             -> AverageLength -- ^ The average length of the dynamic character in the subtree
                             -> c             -- ^ The alignment context of the subtree
                             -> s             -- ^ Resulting decoration


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( DirectOptimizationPostorderDecoration s c
      , DirectOptimizationDecoration s c
      ) => PostorderExtensionDirectOptimizationDecoration s c | s -> c where

    extendPostorderToDirectOptimization :: DirectOptimizationPostorderDecoration x c
                                        => x -- ^ Original decoration
                                        -> c -- ^ The single disambiguation
                                        -> c -- ^ The implied Alignment
                                        -> s -- ^ Resulting decoration


-- |
-- A 'Lens' for the 'characterLocalCost' field
class HasCharacterLocalCost s a | s -> a where

    characterLocalCost :: Lens' s a
    {-# MINIMAL characterLocalCost #-}


-- |
-- A 'Lens' for the 'encoded' field
class HasEncoded s a | s -> a where

    encoded :: Lens' s a
    {-# MINIMAL encoded #-}


-- |
-- A 'Lens' for the 'singleDisambiguation' field
class HasSingleDisambiguation s a | s -> a where

    singleDisambiguation :: Lens' s a
    {-# MINIMAL singleDisambiguation #-}


-- |
-- A 'Lens' for the 'rightAlignment' field
class HasAlignmentContext s a | s -> a where

    alignmentContext :: Lens' s a
    {-# MINIMAL alignmentContext #-}


-- |
-- A 'Lens' for the 'impliedAlignment' field
class HasImpliedAlignment s a | s -> a where

    impliedAlignment :: Lens' s a
    {-# MINIMAL impliedAlignment #-}


-- |
-- A 'Lens' for the 'averageLength' field
class HasAverageLength s a | s -> a where

    averageLength :: Lens' s a
    {-# MINIMAL averageLength #-}



-- |
-- Safely construct an 'AverageLength' from a non-negative value.
toAverageLength :: Word -> AverageLength
toAverageLength = AL . fromNonNegativeValue


-- |
-- Safely convert an 'AverageLength' to a 'Fractional' representation.
getAverageLength :: Fractional a => AverageLength -> a
getAverageLength (AL x) = fromNonNegativeAverage x
