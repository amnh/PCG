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

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Metadata.Dynamic.Class
  ( DenseTransitionCostMatrix
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDenseTransitionCostMatrix(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , HasTraversalFoci(..)
  , MemoizedCostMatrix()
  ) where


--import Bio.Character.Encodable
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise (DenseTransitionCostMatrix)
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.General
import           Control.Lens
import           Data.TCM.Memoized


-- |
-- A 'Lens' for the 'denseTransitionCostMatrix' field
class HasDenseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL denseTransitionCostMatrix #-}
    denseTransitionCostMatrix  :: Lens' s a


-- |
-- A 'Lens' for the 'traversalFoci' field
class HasTraversalFoci s a | s -> a where

    {-# MINIMAL traversalFoci #-}
    traversalFoci  :: Lens' s a
