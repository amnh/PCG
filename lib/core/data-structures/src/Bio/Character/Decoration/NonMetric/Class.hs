-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.NonMetric.Class
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

module Bio.Character.Decoration.NonMetric.Class where


import Bio.Character.Decoration.Discrete


-- |
-- A character optimization class for any TCM; unconstrained by metricity, symmetry, etc.
class DiscreteCharacterDecoration s a => NonMetricCharacterDecoration s a | s -> a where
