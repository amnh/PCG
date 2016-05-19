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
-- Class for nodes with their packed data
-- TODO: Describe what packed data ois, computationally and in a biological context.
-----------------------------------------------------------------------------


{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Node.Packed where

import Data.Vector

-- | A packed node has its packed data
--   TODO: typeclass laws go here for equational reasoning
class PackedNode a s | a -> s where
    -- | What kind of "packed data" am I getting?
    getPacked :: a -> Vector s
    -- | What kind of "packed data" do I give here?
    setPacked :: a -> Vector s -> a
