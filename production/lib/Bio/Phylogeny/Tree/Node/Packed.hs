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
--
-----------------------------------------------------------------------------


{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Packed where

import Data.Vector

-- | A packed node has its packed data
class PackedNode a s | a -> s where
    packed :: a -> Vector s
    setPacked :: a -> Vector s -> a