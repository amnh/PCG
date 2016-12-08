-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.NonMetric.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.NonMetric.Class where


import Bio.Character.Decoration.Discrete


class DiscreteCharacterDecoration s a => NonMetricCharacterDecoration s a | s -> a where
