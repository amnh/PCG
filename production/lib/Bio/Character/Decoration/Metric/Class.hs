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

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Metric.Class where


import Bio.Character.Decoration.Discrete
import Control.Lens


-- |
-- A character optimization class for TCM which satisfy the properties of metricity.
class DiscreteCharacterDecoration s a => MetricCharacterDecoration s a | s -> a where


-- |
-- A decoration containing a character that has been scored using Sankoff's algorithm.
class ( DiscreteCharacterDecoration s c
      , HasMinCostVector s [Word]
      , HasDirectionalMinVector s ([Word], [Word])
      , HasMinCost s Word
      ) => SankoffDecoration s c | s -> c where

  
-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( SankoffDecoration s c
      ) => DiscreteExtensionSankoffDecoration s c | s -> c where

    extendDiscreteToSankoff :: DiscreteCharacterDecoration x c => x -> [Word] -> ([Word], [Word]) -> Word -> s


-- |
-- A 'Lens' for the 'minCostVector' field.
class HasMinCostVector s a | s -> a where

    minCostVector :: Lens' s a
    {-# MINIMAL minCostVector #-}


-- |
-- A 'Lens' for the 'directionalMinVector' field.
class HasDirectionalMinVector s a | s -> a where

    directionalMinVector :: Lens' s a
    {-# MINIMAL directionalMinVector #-}


-- |
-- A 'Lens' for the 'minCost' field.
class HasMinCost s a | s -> a where

    minCost :: Lens' s a
    {-# MINIMAL minCost #-}

