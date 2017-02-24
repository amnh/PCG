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
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteWithTcmCharacterMetadata    s c
      , HasDenseTransitionCostMatrix        s MemoizedCostMatrix
      , HasSparseTransitionCostMatrix       s DenseTransitionCostMatrix
      ) => DynamicCharacterMetadata s c | s -> c where

-- |
-- A 'Lens' for the 'denseTransitionCostMatrix' field
class HasDenseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL denseTransitionCostMatrix #-}
    denseTransitionCostMatrix  :: Lens' s a
