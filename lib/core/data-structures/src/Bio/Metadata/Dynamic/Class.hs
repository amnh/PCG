-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Dynamic.Class
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
  , GetDenseTransitionCostMatrix(..)
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , GetThreewayTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasTraversalFoci(..)
  , MemoizedCostMatrix()
  ) where


import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens.Type            (Getter, Lens')
import Data.TCM.Dense               (DenseTransitionCostMatrix)
import Data.TCM.Memoized


-- |
-- A 'Getter' for the 'denseTransitionCostMatrix' field
class GetDenseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL denseTransitionCostMatrix #-}
    denseTransitionCostMatrix :: Getter s a


-- |
-- A 'Getter' for the 'threewayTransitionCostMatrix' field
class GetThreewayTransitionCostMatrix s a | s -> a where

    {-# MINIMAL threewayTransitionCostMatrix #-}
    threewayTransitionCostMatrix :: Getter s a


-- |
-- A 'Control.Lens.Type.Lens' for the 'traversalFoci' field
class HasTraversalFoci s a | s -> a where

    {-# MINIMAL traversalFoci #-}
    traversalFoci :: Lens' s a
