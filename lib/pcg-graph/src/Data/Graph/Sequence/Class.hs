{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Graph.Sequence.Class where

import Control.Lens

class MetricSpace charSeq where
  dist :: charSeq -> charSeq -> Double

class MedianSpace charSeq where
  median :: charSeq -> charSeq -> charSeq

class HasCharacterSequence s a | s -> a where
  _characterSequence :: Lens' s a
