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

module Bio.PhyloGraph.Node.Preliminary where

import Data.Vector

-- | A preliminary node has a preliminary assignment as well as associated data
-- Associated data: aligned preliminary, temporary, and cost
class PreliminaryNode n s | n -> s where
    getPreliminaryUngapped :: n -> Vector s
    setPreliminaryUngapped :: Vector s -> n -> n
    getPreliminaryGapped   :: n -> Vector s
    setPreliminaryGapped   :: Vector s -> n -> n

    getLeftAlignment       :: n -> Vector s
    setLeftAlignment       :: Vector s -> n -> n
    getRightAlignment      :: n -> Vector s
    setRightAlignment      :: Vector s -> n -> n

    -- getTemporary        :: n -> Vector s
    -- setTemporary     :: Vector s -> n -> n
    getLocalCost           :: n -> Double
    setLocalCost           :: Double -> n -> n
    getTotalCost           :: n -> Double
    setTotalCost           :: Double -> n -> n

