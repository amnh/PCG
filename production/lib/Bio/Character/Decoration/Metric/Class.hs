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


class DiscreteCharacterDecoration s a => MetricCharacterDecoration s a | s -> a where


class ( DiscreteCharacterDecoration s c
      , HasMinCostVector s [Word]
      , HasDirectionalMinVector s ([Word], [Word])
      , HasMinCost s Word
      ) => SankoffDecoration s c | s -> c where

class ( SankoffDecoration s c
      ) => FromExtentionSankoffDecoration s c | s -> c where

    extendToSankoff :: DiscreteCharacterDecoration x c => x -> [Word] -> ([Word], [Word]) -> Word -> s


class HasMinCostVector s a | s -> a where

    minCostVector :: Lens' s a
    {-# MINIMAL minCostVector #-}


class HasDirectionalMinVector s a | s -> a where

    directionalMinVector :: Lens' s a
    {-# MINIMAL directionalMinVector #-}


class HasMinCost s a | s -> a where

    minCost :: Lens' s a
    {-# MINIMAL minCost #-}
    
