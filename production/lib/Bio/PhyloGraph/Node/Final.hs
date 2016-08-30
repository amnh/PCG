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

{- |
  A final node has its final & single assignments.
  Final assingments are the ambiguous result of a direct optimization traversal.
  Single assingments are an arbitrary resolution of the ambiguous Final assignment.
-}
class FinalNode n s | n -> s where

    -- | An getter to the Final /ungapped/ assignments of a node's sequence
    getFinal       :: n -> Vector s

    -- | An setter to the Final /ungapped/ assignments of a node's sequence
    setFinal       :: Vector s -> n -> n

    -- | An getter to the Final /gapped/ assignments of a node's sequence
    getFinalGapped :: n -> Vector s

    -- | An setter to the Final /gapped/ assignments of a node's sequence
    setFinalGapped :: Vector s -> n -> n

    -- | An getter to the Single assignments of a node's sequence
    getSingle      :: n -> Vector s

    -- | An setter to the Single assignments of a node's sequence
    setSingle      :: Vector s -> n -> n 
