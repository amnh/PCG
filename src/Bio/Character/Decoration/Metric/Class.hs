-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Metric.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Character.Decoration.Metric.Class where


import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Shared
import           Control.Lens
import           Numeric.Extended.Natural


-- |
-- A character optimization class for TCMs which satisfy the properties of metricity.
class DiscreteCharacterDecoration s a => MetricCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Sankoff's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasCharacterCost            s Word
      , HasCharacterCostVector      s [ExtendedNatural]
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
                            -> [ExtendedNatural]
                            -> [ExtendedNatural]
                            -> [ExtendedNatural]
                            -> ([[Word]], [[Word]])
                            -> Word
                            -> c
                            -> Bool
                            -> s


-- |
-- A 'Lens' for the 'beta' field.
class HasBeta s a | s -> a where

    {-# MINIMAL beta #-}
    beta :: Lens' s a


-- |
-- A 'Lens' for the 'characterCostVector' field.
class HasCharacterCostVector s a | s -> a where

    {-# MINIMAL characterCostVector #-}
    characterCostVector :: Lens' s a


-- |
-- A 'Lens' for the 'finalExtraCost' field.
class HasFinalExtraCost s a | s -> a where

    {-# MINIMAL finalExtraCost #-}
    finalExtraCost :: Lens' s a


-- |
-- A 'Lens' for the 'preliminaryExtraCost' field.
class HasPreliminaryExtraCost s a | s -> a where

    {-# MINIMAL preliminaryExtraCost #-}
    preliminaryExtraCost :: Lens' s a


-- |
-- A 'Lens' for the 'minStateTuple' field.
class HasStateMinTuple s a | s -> a where

    {-# MINIMAL minStateTuple #-}
    minStateTuple :: Lens' s a
