-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Dynamic.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Metadata.Dynamic.Class
  ( DenseTransitionCostMatrix
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDenseTransitionCostMatrix(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , HasTraversalLoci(..)
  , MemoizedCostMatrix()
  ) where

--import Bio.Character.Encodable
import Analysis.Parsimony.Dynamic.DirectOptimization.FFI (DenseTransitionCostMatrix)
import Analysis.Parsimony.Dynamic.SequentialAlign.FFI    (MemoizedCostMatrix)
import Bio.Metadata.General
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens


-- |
-- A 'Lens' for the 'denseTransitionCostMatrix' field
class HasDenseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL denseTransitionCostMatrix #-}
    denseTransitionCostMatrix  :: Lens' s a


-- |
-- A 'Lens' for the 'denseTransitionCostMatrix' field
class HasTraversalLoci s a | s -> a where

    {-# MINIMAL traversalLoci #-}
    traversalLoci  :: Lens' s a
