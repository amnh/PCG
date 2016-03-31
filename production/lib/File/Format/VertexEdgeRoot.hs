-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.VertexEdgeRoot
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing VER files into a collection of the
-- vertex set, edge set, and root set representing a "Phylogenetic Forest".
--
-----------------------------------------------------------------------------

module File.Format.VertexEdgeRoot
  ( VertexEdgeRoot(..)
  , VertexLabel
  , verStreamParser
  , edgeConnection
  , edgeOrigin
  , edgeTarget
  , edgeLength
  ) where

import File.Format.VertexEdgeRoot.Parser
