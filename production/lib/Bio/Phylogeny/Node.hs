-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for the representation of a node
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Phylogeny.Node (Node(..)
                                , module Bio.Phylogeny.Node.Encoded
                                , module Bio.Phylogeny.Node.Final
                                , module Bio.Phylogeny.Node.Packed
                                , module Bio.Phylogeny.Node.Preliminary
                                , module Bio.Phylogeny.Node.Internal
                                ) where

import Bio.Phylogeny.Node.Encoded 
import Bio.Phylogeny.Node.Final 
import Bio.Phylogeny.Node.Packed 
import Bio.Phylogeny.Node.Preliminary 
import Bio.Phylogeny.Node.Internal
