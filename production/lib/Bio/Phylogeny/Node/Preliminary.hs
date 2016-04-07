-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for nodes with preliminary assignment data
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Node.Preliminary where

import Data.Vector

-- | A preliminary node has a preliminary assignment as well as associated data
-- Associated data: aligned preliminary, temporary, and cost
class PreliminaryNode n s | n -> s where
    preliminary      :: n -> Vector s
    setPreliminary   :: Vector s -> n -> n
    preliminaryAlign :: n -> Vector s
    setAlign         :: Vector s -> n -> n
    temporary        :: n -> Vector s
    setTemporary     :: Vector s -> n -> n
    localCost        :: n -> Double
    setLocalCost     :: Double -> n -> n
    totalCost        :: n -> Double
    setTotalCost     :: Double -> n -> n
