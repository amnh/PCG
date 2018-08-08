-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TransitionCostMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing TCM files into an alphabet and native matrix form.
--
-----------------------------------------------------------------------------

module File.Format.TransitionCostMatrix
  ( TCM(..)
  , tcmStreamParser
  ) where


import           File.Format.TransitionCostMatrix.Parser
