-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Bio.Sequence
  (
  -- * CharacterSequence types
    CharacterBlock()
  , CharacterSequence()
  -- * MetadataSequence types
  , MetadataBlock()
  , MetadataSequence()
  -- * Construction types
  , PartialCharacterBlock()
  -- * Sequence constraints
  , HasBlockCost
  , HasRootCost
  -- * Lenses
  , HasBlocks(..)
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
  -- * CharacterBlock construction
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  , finalizeCharacterBlock
  -- * CharacterBlock transformations
  , toMissingCharacters
  , foldZipWithMeta
  , hexFold
  , hexmap
  , hexZip
  , hexZipWith
  , hexZipMeta
  -- * Cost quantification
  , sequenceCost
  , sequenceRootCost
  , blockCost
  , staticCost
  ) where

import           Bio.Character.Encodable
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence.Block           hiding (hexZipMeta, hexZipWith, hexZipWithMeta, hexmap)
import qualified Bio.Sequence.Block           as Blk
import           Bio.Sequence.Block.Builder
import           Bio.Sequence.Block.Character (finalizeCharacterBlock)
import           Bio.Sequence.Character       (CharacterSequence)
import           Bio.Sequence.Internal
import           Bio.Sequence.Metadata        (MetadataSequence)
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable.Custom
import           Data.Key
import           Data.List.Utility
import           Data.MonoTraversable
import           Prelude                      hiding (zip)


-- |
-- Perform a six way map over the polymorphic types.
hexmap
  :: (u -> u')
  -> (v -> v')
  -> (w -> w')
  -> (x -> x')
  -> (y -> y')
  -> (z -> z')
  -> CharacterSequence u  v  w  x  y  z
  -> CharacterSequence u' v' w' x' y' z'
hexmap f1 f2 f3 f4 f5 f6 =
    over blockSequence (parmap rpar (Blk.hexmap f1 f2 f3 f4 f5 f6))


-- |
-- Zips together two 'CharacterSequence' values to get pairs of values.
hexZip
  :: CharacterSequence u v w x y z
  -> CharacterSequence u' v' w' x' y' z'
  -> CharacterSequence (u,u') (v,v') (w,w') (x,x') (y,y') (z,z')
hexZip = hexZipWith (,) (,) (,) (,) (,) (,)


-- |
-- Performs a zip over the two character sequences. Uses the input functions to
-- zip the different character types in the character block.
--
-- Assumes that the 'CharacterSequence' values have the same number of character
-- blocks and the same number of each character type in the corresponding block
-- of each block. If this assumtion is violated, the result will be truncated.
hexZipWith
  :: (u -> u' -> u'')
  -> (v -> v' -> v'')
  -> (w -> w' -> w'')
  -> (x -> x' -> x'')
  -> (y -> y' -> y'')
  -> (z -> z' -> z'')
  -> CharacterSequence u   v   w   x   y   z
  -> CharacterSequence u'  v'  w'  x'  y'  z'
  -> CharacterSequence u'' v'' w'' x'' y'' z''
hexZipWith f1 f2 f3 f4 f5 f6 lhs =
    over blockSequence (parZipWith rpar (Blk.hexZipWith f1 f2 f3 f4 f5 f6) (lhs ^. blockSequence))


-- |
-- Performs a zip over a character sequence and a metadata sequence.
--
-- Assumes that the 'CharacterSequence' and 'MetadataSequence' have the same
-- number of character blocks and the same number of each character type in
-- the corresponding block of each block. If this assumtion is violated, the
-- result will be truncated.
hexZipMeta
  :: (ContinuousCharacterMetadataDec                      -> u -> u')
  -> (DiscreteCharacterMetadataDec                        -> v -> v')
  -> (DiscreteCharacterMetadataDec                        -> w -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> x -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> y -> y')
  -> (DynamicCharacterMetadataDec (Subcomponent (Element DynamicCharacter)) -> z -> z')
  -> MetadataSequence m
  -> CharacterSequence u   v   w   x   y   z
  -> CharacterSequence u'  v'  w'  x'  y'  z'
hexZipMeta f1 f2 f3 f4 f5 f6 meta =
    over blockSequence (parZipWith rpar (Blk.hexZipMeta f1 f2 f3 f4 f5 f6) mSeq)
  where
    mSeq = meta ^. blockSequence


-- |
-- Calculates the cumulative cost of a 'CharacterSequence'. Performs some of the
-- operation in parallel.
sequenceCost
  :: HasBlockCost u v w x y z
  => MetadataSequence m
  -> CharacterSequence u v w x y z
  -> Double
sequenceCost meta char = sum'
    . parmap rpar (uncurry Blk.blockCost)
    . zip (meta ^. blockSequence) $ char ^. blockSequence


-- |
-- Calculates the root cost of a 'CharacterSequence'. Performs some of the
-- operation in parallel.
sequenceRootCost
  :: (HasRootCost u v w x y z, Integral i)
  => i
  -> MetadataSequence m
  -> CharacterSequence u v w x y z
  -> Double
sequenceRootCost rootCount meta char = sum'
    . parmap rpar (uncurry (Blk.rootCost rootCount))
    . zip (meta ^. blockSequence) $ char ^. blockSequence



foldZipWithMeta
  :: (Monoid a)
  => (MetadataBlock m -> CharacterBlock u v w x y z -> CharacterBlock u v w x y z -> a)
  -> MetadataSequence m
  -> CharacterSequence u v w x y z
  -> CharacterSequence u v w x y z
  -> a
foldZipWithMeta f meta charSeq1 charSeq2 =
  let metaV     = otoList meta
      charSeq1V = otoList charSeq1
      charSeq2V = otoList charSeq2
  in  foldZipWith3 f metaV charSeq1V charSeq2V
