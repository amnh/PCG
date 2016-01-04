-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Multi.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for holding a MultiSequence, or a series of sequences like those stored at a node
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Multi.Class where

class MultiSequence i s | i -> s where
    filterMulti :: i -> (s -> Bool) -> i
