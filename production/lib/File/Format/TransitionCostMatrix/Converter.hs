-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TransitionCostMatrix.Converter
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing TCM files into an alphabet and square matrix.
--
-----------------------------------------------------------------------------

module File.Format.TransitionCostMatrix.Converter where

import Bio.Phylogeny.Graph

import File.Format.TransitionCostMatrix.Parser

incorporateTCM :: Graph -> Graph
incorporateTCM = undefined