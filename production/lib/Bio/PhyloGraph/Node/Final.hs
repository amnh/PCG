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
-- Class for nodes with a final assignment
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Node.Final where

import Data.Vector

-- | A final node has its final assignment
class FinalNode n s | n -> s where
    getFinal :: n -> Vector s
    setFinal :: Vector s -> n -> n
    getFinalGapped :: n -> Vector s
    setFinalGapped :: Vector s -> n -> n
