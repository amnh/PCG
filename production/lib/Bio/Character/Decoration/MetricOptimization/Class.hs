-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.MetricOptimization.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Secondary Sankoff model for speeding up search using techniques from
-- Goloboff 1998, "Tree Search ... Sankoff". Based on Metric, but additional
-- fields added to deal with equations presented in Goloboff paper.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.MetricOptimization.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens
import Data.ExtendedNatural


-- |
-- A character optimization class for TCM which satisfy the properties of metricity.
class DiscreteCharacterDecoration s a => MetricOptimizationCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Sankoff's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasCharacterCost            s Word
      , HasCharacterCostVector      s  [ExtendedNatural]
      , HasStateMinTuple            s ([[Word]], [[Word]])
      ) => SankoffDecoration s c | s -> c where


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( SankoffDecoration s c
      ) => DiscreteExtensionSankoffDecoration s c | s -> c where

    extendDiscreteToSankoff :: DiscreteCharacterDecoration x c
                            => x
                            -> [ExtendedNatural]
                            -> ([[Word]], [[Word]])
                            -> Word
                            -> c
                            -> Bool
                            -> s


-- |
-- A 'Lens' for the 'minCostVector' field.
class HasCharacterCostVector s a | s -> a where

    {-# MINIMAL characterCostVector #-}
    characterCostVector :: Lens' s a


-- |
-- A 'Lens' for the 'minStateTuple' field.
class HasStateMinTuple s a | s -> a where

    {-# MINIMAL minStateTuple #-}
    minStateTuple :: Lens' s a

