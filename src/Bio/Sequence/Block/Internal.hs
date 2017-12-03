------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Internal
  ( CharacterBlock(..)
  ) where


import Control.DeepSeq
import Data.Bifunctor
import Data.Foldable
import Data.Semigroup
import Data.Vector            (Vector)
import Data.Vector.Instances  ()
import GHC.Generics
import Prelude         hiding (zipWith)
import Text.XML
-- import Text.XML.Light.Types


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Bin types:
-- * u =
-- * v =
-- * w =
-- * x =
-- * y =
-- * z =
--
-- Use '(<>)' to construct larger blocks.
data CharacterBlock u v w x y z
   = CharacterBlock
   { continuousCharacterBins  :: Vector u
   , nonAdditiveCharacterBins :: Vector v
   , additiveCharacterBins    :: Vector w
   , metricCharacterBins      :: Vector x
   , nonMetricCharacterBins   :: Vector y
   , dynamicCharacters        :: Vector z
   } deriving (Eq, Generic)


instance Bifunctor (CharacterBlock u v w x) where

    bimap f g =
        CharacterBlock
          <$> continuousCharacterBins
          <*> nonAdditiveCharacterBins
          <*> additiveCharacterBins
          <*> metricCharacterBins
          <*> fmap f . nonMetricCharacterBins
          <*> fmap g . dynamicCharacters

    first f =
        CharacterBlock
          <$> continuousCharacterBins
          <*> nonAdditiveCharacterBins
          <*> additiveCharacterBins
          <*> metricCharacterBins
          <*> fmap f . nonMetricCharacterBins
          <*> dynamicCharacters

    second = fmap


instance Functor (CharacterBlock u v w x y) where

    fmap f =
        CharacterBlock
          <$> continuousCharacterBins
          <*> nonAdditiveCharacterBins
          <*> additiveCharacterBins
          <*> metricCharacterBins
          <*> nonMetricCharacterBins
          <*> fmap f . dynamicCharacters

    (<$) v =
        CharacterBlock
          <$> continuousCharacterBins
          <*> nonAdditiveCharacterBins
          <*> additiveCharacterBins
          <*> metricCharacterBins
          <*> nonMetricCharacterBins
          <*> (v <$) . dynamicCharacters


instance (NFData u, NFData v, NFData w, NFData x, NFData y, NFData z) => NFData (CharacterBlock u v w x y z)


-- | (✔)
instance Semigroup (CharacterBlock u v w x y z) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins  = continuousCharacterBins  lhs <> continuousCharacterBins  rhs
          , nonAdditiveCharacterBins = nonAdditiveCharacterBins lhs <> nonAdditiveCharacterBins rhs
          , additiveCharacterBins    = additiveCharacterBins    lhs <> additiveCharacterBins    rhs
          , metricCharacterBins      = metricCharacterBins      lhs <> metricCharacterBins      rhs
          , nonMetricCharacterBins   = nonMetricCharacterBins   lhs <> nonMetricCharacterBins   rhs
          , dynamicCharacters        = dynamicCharacters        lhs <> dynamicCharacters        rhs
          }


-- | (✔)
instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (CharacterBlock u v w x y z) where

    show block = unlines
        [ "Non-additive Characters:"
        , niceRendering $ nonAdditiveCharacterBins block
        , "Additive Characters:"
        , niceRendering $ additiveCharacterBins block
        , "NonMetric Characters:"
        , niceRendering $ nonMetricCharacterBins block
        , "Continuous Characters: "
        , niceRendering $ continuousCharacterBins block
        , "Metric Characters:"
        , niceRendering $ metricCharacterBins block
        , "Dynamic Characters:"
        , niceRendering $ dynamicCharacters block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList


-- | (✔)
instance ( ToXML u -- This is NOT a redundant constraint.
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterBlock u v w x y z) where

    toXML block = xmlElement "Character_block" attributes contents
        where
            attributes = []
            contents   = [ Right . collapseElemList "Non-additive_character_block" [] $ nonAdditiveCharacterBins block
                         , Right . collapseElemList "Additive_character_block"     [] $ additiveCharacterBins    block
                         , Right . collapseElemList "NonMetric_character_block"    [] $ nonMetricCharacterBins   block
                         , Right . collapseElemList "Continuous_character_block"   [] $ continuousCharacterBins  block
                         , Right . collapseElemList "Metric_character_block"       [] $ nonMetricCharacterBins   block
                         , Right . collapseElemList "Dynamic_character_block"      [] $ dynamicCharacters        block
                         ]
