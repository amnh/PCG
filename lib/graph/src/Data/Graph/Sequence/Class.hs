------------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Sequence.Class
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Graph.Sequence.Class
  ( CharacterSequence
  , FinalDecoration
  , HasCharacterSequence(..)
  , MetricSpace(..)
  , MedianSpace(..)
  ,
  ) where

import Control.Lens.Type (Lens')
import Data.Kind


-- |
-- Type-family defining the character sequence of a node.
type family CharacterSequence a :: Type


-- |
-- Type-family defining the final decoration of a node.
type family FinalDecoration a :: Type


-- |
-- Abstraction for comparing the distance between two character sequences.
class MetricSpace charSeq where

    dist :: charSeq -> charSeq -> Double


-- |
-- Abstraction for computing the median sequence between two sequences.
class MedianSpace charSeq where

    median :: charSeq -> charSeq -> charSeq


-- |
-- A 'Control.Lens.Type.Lens' for object which have a character sequence.
class HasCharacterSequence s a | s -> a where

    _characterSequence :: Lens' s a


