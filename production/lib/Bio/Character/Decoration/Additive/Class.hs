-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Additive.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Additive.Class where


import Bio.Character.Decoration.Discrete


class DiscreteCharacterDecoration s a => AdditiveCharacterDecoration s a | s -> a where
