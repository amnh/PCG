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

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Metric.Class where


import Bio.Character.Decoration.Discrete


class DiscreteCharacterDecoration s a => MetricCharacterDecoration s a | s -> a where
