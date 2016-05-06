-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A Solution carrying metadata
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.PhyloGraph.Solution.Metadata where

import Data.Vector

-- | A type from which ordered metadata retreived and stored.
class MetadataSolution r m | r -> m where
    getMetadata :: r -> Vector m
    setMetadata :: r -> Vector m -> r
