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


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Metadata.CharacterName
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Key
import           Data.Monoid                          (mappend)
import           Data.Semigroup
--import           Data.Semigroup.Traversable
import           Data.TCM
import           Data.Vector                          (Vector)
import           Data.Vector.Instances                ()
import qualified Data.Vector                   as V
import           Prelude                       hiding (zipWith)
import           Safe                                 (headMay)


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
          { continuousCharacterBins  = continuousCharacterBins  lhs `mappend` continuousCharacterBins  rhs
          , nonAdditiveCharacterBins = nonAdditiveCharacterBins lhs `mappend` nonAdditiveCharacterBins rhs
          , additiveCharacterBins    = additiveCharacterBins    lhs `mappend` additiveCharacterBins    rhs
          , metricCharacterBins      = metricCharacterBins      lhs `mappend` metricCharacterBins      rhs
          , nonMetricCharacterBins   = nonMetricCharacterBins   lhs `mappend` nonMetricCharacterBins   rhs
          , dynamicCharacters        = dynamicCharacters        lhs `mappend` dynamicCharacters        rhs
          }
          

instance ( Show m
         , Show i
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (CharacterBlock m i c f a d) where

    show block = unlines
        [ "Continuous Characters: "
        , niceRendering $ continuousCharacterBins block
        , "Fitch Characters:"
        , niceRendering $ nonAdditiveCharacterBins block
        , "Additive Characters:"
        , niceRendering $ additiveCharacterBins block
        , "Metric Characters:"
        , niceRendering $ metricCharacterBins block
        , "NonMetric Characters:"
        , niceRendering $ nonMetricCharacterBins block
        , "Dynamic Characters:"
        , niceRendering $ dynamicCharacters block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList

