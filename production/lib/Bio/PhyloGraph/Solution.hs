-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a solution representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.Solution
  ( module Bio.PhyloGraph.Solution.Internal
  , module Bio.PhyloGraph.Solution.Class
  ) where

import           Bio.PhyloGraph.Forest           
import           Bio.PhyloGraph.Solution.Class
import qualified Bio.PhyloGraph.Solution.Metadata as MS
import           Bio.PhyloGraph.Solution.Internal

import Data.Monoid

-- | Make it an instance of data storage type classes

instance GeneralSolution (Solution d) (Forest d) where
    getForests = forests
    setForests s f = s {forests = f} 

instance MS.MetadataSolution (Solution d) StandardMetadata where
    metadata = metadata
    setMetadata solution meta = solution {metadata = meta}

instance Monoid (Solution d) where
    mempty = Solution mempty mempty mempty
    mappend (Solution chars1 meta1 forests1) (Solution chars2 meta2 forests2) = 
        Solution (chars1 <> chars2) (meta1 <> meta2) (forests1 <> forests2)
