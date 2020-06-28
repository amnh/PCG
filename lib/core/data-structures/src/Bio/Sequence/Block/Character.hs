------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Character
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UnboxedSums           #-}


module Bio.Sequence.Block.Character
  ( CharacterBlock()
  -- * Lenses
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
  -- * Block construction
  , finalizeCharacterBlock
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  -- * Transformations
  , hexmap
  , hexZipMeta
  , hexZipWith
  , hexZipWithMeta
  , toMissingCharacters
  ) where

import           Bio.Character.Encodable
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence.Block.Builder
import           Bio.Sequence.Block.Internal
import           Bio.Sequence.Block.Metadata  (MetadataBlock)
import           Control.DeepSeq
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.MonoTraversable         (Element)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Text                    as T (Text, lines, unlines)
import           Data.Vector                  (Vector)
import           Data.Vector.Instances        ()
import           GHC.Generics
import           Text.XML
import           TextShow                     (TextShow (showb, showt), fromText)
import qualified VectorBuilder.Vector         as VB


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
data  CharacterBlock u v w x y z
    = BlockDoesNotExist
    | CB {-# UNPACK #-} !(Block u v w x y z)
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)


instance HasContinuousBin (CharacterBlock u v w x y z) (Vector u) where

    {-# INLINE continuousBin #-}
    continuousBin = lens (unwrapWith _continuousBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _continuousBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then cb
                         else CB Block
                                 { _continuousBin  = v
                                 , _nonAdditiveBin = mempty
                                 , _additiveBin    = mempty
                                 , _metricBin      = mempty
                                 , _nonMetricBin   = mempty
                                 , _dynamicBin     = mempty
                                 }


instance HasNonAdditiveBin (CharacterBlock u v w x y z) (Vector v) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens (unwrapWith _nonAdditiveBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _nonAdditiveBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then cb
                         else CB Block
                                 { _continuousBin  = mempty
                                 , _nonAdditiveBin = v
                                 , _additiveBin    = mempty
                                 , _metricBin      = mempty
                                 , _nonMetricBin   = mempty
                                 , _dynamicBin     = mempty
                                 }


instance HasAdditiveBin (CharacterBlock u v w x y z) (Vector w) where

    {-# INLINE additiveBin #-}
    additiveBin = lens (unwrapWith _additiveBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _additiveBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then cb
                         else CB Block
                                 { _continuousBin  = mempty
                                 , _nonAdditiveBin = mempty
                                 , _additiveBin    = v
                                 , _metricBin      = mempty
                                 , _nonMetricBin   = mempty
                                 , _dynamicBin     = mempty
                                 }


instance HasMetricBin (CharacterBlock u v w x y z) (Vector x) where

    {-# INLINE metricBin #-}
    metricBin = lens (unwrapWith _metricBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _metricBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then cb
                         else CB Block
                                 { _continuousBin  = mempty
                                 , _nonAdditiveBin = mempty
                                 , _additiveBin    = mempty
                                 , _metricBin      = v
                                 , _nonMetricBin   = mempty
                                 , _dynamicBin     = mempty
                                 }


instance HasNonMetricBin (CharacterBlock u v w x y z) (Vector y) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens (unwrapWith _nonMetricBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _nonMetricBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then cb
                         else CB Block
                                 { _continuousBin  = mempty
                                 , _nonAdditiveBin = mempty
                                 , _additiveBin    = mempty
                                 , _metricBin      = mempty
                                 , _nonMetricBin   = v
                                 , _dynamicBin     = mempty
                                 }


instance HasDynamicBin (CharacterBlock u v w x y z) (CharacterBlock u v w x y z') (Vector z) (Vector z') where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens (unwrapWith _dynamicBin)
        $ \cb v -> case cb of
                     (CB b)            -> CB (b { _dynamicBin = v })
                     BlockDoesNotExist ->
                         if null v
                         then BlockDoesNotExist
                         else CB Block
                                 { _continuousBin  = mempty
                                 , _nonAdditiveBin = mempty
                                 , _additiveBin    = mempty
                                 , _metricBin      = mempty
                                 , _nonMetricBin   = mempty
                                 , _dynamicBin     = v
                                 }


instance Bifunctor (CharacterBlock u v w x) where

    bimap f g (CB b) = CB $
        (Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> fmap g . _dynamicBin
        ) b
    bimap _ _ _ = BlockDoesNotExist

    first f (CB b) = CB $
        (Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> fmap f . _nonMetricBin
          <*> _dynamicBin
        ) b
    first _ _ = BlockDoesNotExist

    second = fmap


instance Functor (CharacterBlock u v w x y) where

    fmap f (CB b) = CB $ f <$> b
    fmap _  _     = BlockDoesNotExist

    (<$) v (CB b) = CB $
        (Block
          <$> _continuousBin
          <*> _nonAdditiveBin
          <*> _additiveBin
          <*> _metricBin
          <*> _nonMetricBin
          <*> (v <$) . _dynamicBin
        ) b
    (<$) _ _ = BlockDoesNotExist


instance Semigroup (CharacterBlock u v w x y z) where

    (<>) BlockDoesNotExist rhs = rhs
    (<>) lhs BlockDoesNotExist = lhs
    (<>) (CB lhs) (CB rhs)     = CB $ lhs <> rhs

    sconcat cbs =
        case foldMap unwrapCharacterBlock cbs of
          []   -> BlockDoesNotExist
          x:xs -> CB . fold1 $ x:|xs
      where
        unwrapCharacterBlock (CB b) = [b]
        unwrapCharacterBlock  _     = []

    stimes i _ | i < 1 = error $ fold
        [ "Call to Bio.Sequence.CharacterBlock.stimes with non-positive value: "
        , show (fromIntegral i :: Integer)
        , " <= 0"
        ]
    stimes _ BlockDoesNotExist = BlockDoesNotExist
    stimes i (CB b) = CB $ stimes i b


instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (CharacterBlock u v w x y z) where

    show BlockDoesNotExist = "Block does not exists at this node"
    show (CB block) = unlines
        [ "Continuous s [" <> show (length (_continuousBin block)) <> "]:"
        , niceRendering $ _continuousBin block
        , "Non-additive s [" <> show (length (_nonAdditiveBin block)) <> "]:"
        , niceRendering $ _nonAdditiveBin block
        , "Additive s [" <> show (length (_additiveBin block)) <> "]:"
        , niceRendering $ _additiveBin block
        , "Metric s [" <> show (length (_metricBin block)) <> "]:"
        , niceRendering $ _metricBin block
        , "NonMetric s [" <> show (length (_nonMetricBin block)) <> "]:"
        , niceRendering $ _nonMetricBin block
        , "Dynamic s [" <> show (length (_dynamicBin block)) <> "]:"
        , niceRendering $ _dynamicBin block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList

instance ( TextShow u
         , TextShow v
         , TextShow w
         , TextShow x
         , TextShow y
         , TextShow z
         ) => TextShow (CharacterBlock u v w x y z) where

    showb BlockDoesNotExist = "Block does not exists at this node"
    showb (CB block) = fromText . T.unlines $
        [ "Continuous s [" <> showt (length (_continuousBin block)) <> "]:"
        , niceRendering $ _continuousBin block
        , "Non-additive s [" <> showt (length (_nonAdditiveBin block)) <> "]:"
        , niceRendering $ _nonAdditiveBin block
        , "Additive s [" <> showt (length (_additiveBin block)) <> "]:"
        , niceRendering $ _additiveBin block
        , "Metric s [" <> showt (length (_metricBin block)) <> "]:"
        , niceRendering $ _metricBin block
        , "NonMetric s [" <> showt (length (_nonMetricBin block)) <> "]:"
        , niceRendering $ _nonMetricBin block
        , "Dynamic s [" <> showt (length (_dynamicBin block)) <> "]:"
        , niceRendering $ _dynamicBin block
        ]
      where
        niceRendering :: (Foldable t, TextShow a) => t a -> T.Text
        niceRendering = T.unlines . fmap (T.unlines . fmap ("  " <>) . T.lines . showt) . toList


instance ( ToXML u
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterBlock u v w x y z) where

    toXML BlockDoesNotExist = xmlElement "_block does not exist" [] []
    toXML (CB block) = xmlElement "_block" attributes contents
        where
            attributes = []
            contents   = [ Right . collapseElemList "Continuous_character_block"   [] . (^.  continuousBin)
                         , Right . collapseElemList "Non-additive_character_block" [] . (^. nonAdditiveBin)
                         , Right . collapseElemList "Additive_character_block"     [] . (^.    additiveBin)
                         , Right . collapseElemList "NonMetric_character_block"    [] . (^.   nonMetricBin)
                         , Right . collapseElemList "Metric_character_block"       [] . (^.   nonMetricBin)
                         , Right . collapseElemList "Dynamic_character_block"      [] . (^.     dynamicBin)
                         ] <*> [block]


-- |
-- Converts a 'PartialCharacterBlock' to a 'CharacterBlock', finalizing the
-- efficient construction process.
finalizeCharacterBlock :: PartialCharacterBlock u v w x y z -> CharacterBlock u v w x y z
finalizeCharacterBlock =
  CB .
    (Block
      <$> VB.build . partialContinuousCharacterBins
      <*> VB.build . partialNonAdditiveCharacterBins
      <*> VB.build . partialAdditiveCharacterBins
      <*> VB.build . partialMetricCharacterBins
      <*> VB.build . partialNonMetricCharacterBins
      <*> VB.build . partialDynamicCharacters
    )


-- |
-- Perform a six way map over the polymorphic types.
hexmap
 :: (u -> u')
 -> (v -> v')
 -> (w -> w')
 -> (x -> x')
 -> (y -> y')
 -> (z -> z')
 -> CharacterBlock u  v  w  x  y  z
 -> CharacterBlock u' v' w' x' y' z'
hexmap f1 f2 f3 f4 f5 f6 = CB . (
    Block
      <$> (parmap rpar f1 . (^.  continuousBin))
      <*> (parmap rpar f2 . (^. nonAdditiveBin))
      <*> (parmap rpar f3 . (^.    additiveBin))
      <*> (parmap rpar f4 . (^.      metricBin))
      <*> (parmap rpar f5 . (^.   nonMetricBin))
      <*> (parmap rpar f6 . (^.     dynamicBin))
    )


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
--
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipWith
  :: (u -> u' -> u'')
  -> (v -> v' -> v'')
  -> (w -> w' -> w'')
  -> (x -> x' -> x'')
  -> (y -> y' -> y'')
  -> (z -> z' -> z'')
  -> CharacterBlock u   v   w   x   y   z
  -> CharacterBlock u'  v'  w'  x'  y'  z'
  -> CharacterBlock u'' v'' w'' x'' y'' z''
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs = CB
    Block
      { _continuousBin  = parZipWith rpar f1 (lhs ^.  continuousBin) (rhs ^.  continuousBin)
      , _nonAdditiveBin = parZipWith rpar f2 (lhs ^. nonAdditiveBin) (rhs ^. nonAdditiveBin)
      , _additiveBin    = parZipWith rpar f3 (lhs ^.    additiveBin) (rhs ^.    additiveBin)
      , _metricBin      = parZipWith rpar f4 (lhs ^.      metricBin) (rhs ^.      metricBin)
      , _nonMetricBin   = parZipWith rpar f5 (lhs ^.   nonMetricBin) (rhs ^.   nonMetricBin)
      , _dynamicBin     = parZipWith rpar f6 (lhs ^.     dynamicBin) (rhs ^.     dynamicBin)
      }


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
--
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipWithMeta
  :: (ContinuousCharacterMetadataDec                      -> u -> u' -> u'')
  -> (DiscreteCharacterMetadataDec                        -> v -> v' -> v'')
  -> (DiscreteCharacterMetadataDec                        -> w -> w' -> w'')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> x -> x' -> x'')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> y -> y' -> y'')
  -> (DynamicCharacterMetadataDec (Subcomponent (Element DynamicCharacter))   -> z -> z' -> z'')
  -> MetadataBlock m
  -> CharacterBlock u   v   w   x   y   z
  -> CharacterBlock u'  v'  w'  x'  y'  z'
  -> CharacterBlock u'' v'' w'' x'' y'' z''
hexZipWithMeta f1 f2 f3 f4 f5 f6 meta lhs rhs = CB
    Block
      { _continuousBin  = parZipWith3 rpar f1 (meta ^.  continuousBin) (lhs ^.  continuousBin) (rhs ^.  continuousBin)
      , _nonAdditiveBin = parZipWith3 rpar f2 (meta ^. nonAdditiveBin) (lhs ^. nonAdditiveBin) (rhs ^. nonAdditiveBin)
      , _additiveBin    = parZipWith3 rpar f3 (meta ^.    additiveBin) (lhs ^.    additiveBin) (rhs ^.    additiveBin)
      , _metricBin      = parZipWith3 rpar f4 (meta ^.      metricBin) (lhs ^.      metricBin) (rhs ^.      metricBin)
      , _nonMetricBin   = parZipWith3 rpar f5 (meta ^.   nonMetricBin) (lhs ^.   nonMetricBin) (rhs ^.   nonMetricBin)
      , _dynamicBin     = parZipWith3 rpar f6 (meta ^.     dynamicBin) (lhs ^.     dynamicBin) (rhs ^.     dynamicBin)
      }

-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
--
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipMeta
  :: (ContinuousCharacterMetadataDec                         -> u -> u')
  -> (DiscreteCharacterMetadataDec                           -> v -> v')
  -> (DiscreteCharacterMetadataDec                           -> w -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> x -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter    -> y -> y')
  -> (DynamicCharacterMetadataDec (Subcomponent (Element DynamicCharacter)) -> z -> z')
  -> MetadataBlock m
  -> CharacterBlock u  v  w  x  y  z
  -> CharacterBlock u' v' w' x' y' z'
hexZipMeta f1 f2 f3 f4 f5 f6 meta charBlock = CB
    Block
      { _continuousBin
          = parZipWith rpar f1 (meta ^.  continuousBin) (charBlock ^.  continuousBin)
      , _nonAdditiveBin
          = parZipWith rpar f2 (meta ^. nonAdditiveBin) (charBlock ^. nonAdditiveBin)
      , _additiveBin
          = parZipWith rpar f3 (meta ^.    additiveBin) (charBlock ^.    additiveBin)
      , _metricBin
          = parZipWith rpar f4 (meta ^.      metricBin) (charBlock ^.      metricBin)
      , _nonMetricBin
          = parZipWith rpar f5 (meta ^.   nonMetricBin) (charBlock ^.   nonMetricBin)
      , _dynamicBin
          = parZipWith rpar f6 (meta ^.     dynamicBin) (charBlock ^.     dynamicBin)
      }


-- |
-- Convert all characters contained in the block to their missing value.
toMissingCharacters
  :: ( PossiblyMissingCharacter u
     , PossiblyMissingCharacter v
     , PossiblyMissingCharacter w
     , PossiblyMissingCharacter x
     , PossiblyMissingCharacter y
     , PossiblyMissingCharacter z
     )
  => CharacterBlock u v w x y z
  -> CharacterBlock u v w x y z
toMissingCharacters = CB . (
    Block
      <$> (fmap toMissing . (^.  continuousBin))
      <*> (fmap toMissing . (^. nonAdditiveBin))
      <*> (fmap toMissing . (^.    additiveBin))
      <*> (fmap toMissing . (^.      metricBin))
      <*> (fmap toMissing . (^.   nonMetricBin))
      <*> (fmap toMissing . (^.     dynamicBin))
    )


{-# INLINE unwrapWith #-}
unwrapWith :: Monoid a => (Block u v w x y z -> a) -> CharacterBlock u v w x y z -> a
unwrapWith _ BlockDoesNotExist = mempty
unwrapWith f (CB block)        = f block
