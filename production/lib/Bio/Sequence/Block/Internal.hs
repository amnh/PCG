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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Internal
  ( CharacterBlock(..)
  ) where


import Data.Foldable
import Data.Semigroup
import Data.Vector                  (Vector)
import Data.Vector.Instances        ()
import Prelude               hiding (zipWith)


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Use '(<>)' to construct larger blocks.
data CharacterBlock m i c f a d
   = CharacterBlock
   { continuousCharacterBins  :: Vector c
   , nonAdditiveCharacterBins :: Vector f
   , additiveCharacterBins    :: Vector a
   , metricCharacterBins      :: Vector m
   , nonMetricCharacterBins   :: Vector i
   , dynamicCharacters        :: Vector d
   } deriving (Eq)


instance Semigroup (CharacterBlock m i c f a d) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins  = continuousCharacterBins  lhs <> continuousCharacterBins  rhs
          , nonAdditiveCharacterBins = nonAdditiveCharacterBins lhs <> nonAdditiveCharacterBins rhs
          , additiveCharacterBins    = additiveCharacterBins    lhs <> additiveCharacterBins    rhs
          , metricCharacterBins      = metricCharacterBins      lhs <> metricCharacterBins      rhs
          , nonMetricCharacterBins   = nonMetricCharacterBins   lhs <> nonMetricCharacterBins   rhs
          , dynamicCharacters        = dynamicCharacters        lhs <> dynamicCharacters        rhs
          }


instance ( Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (CharacterBlock m i c f a d) where

    show block = unlines
        [ "Fitch Characters:"
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

