{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Graph.Sequence.Class where

import Control.Lens
import Data.Kind

class MetricSpace charSeq where
  dist :: charSeq -> charSeq -> Double

class MedianSpace charSeq where
  median :: charSeq -> charSeq -> charSeq

class HasCharacterSequence s a | s -> a where
  _characterSequence :: Lens' s a

type family FinalDecoration a :: Type
type family CharacterSequence a :: Type

