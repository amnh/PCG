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
-- Class for nodes with encoded data
--
-----------------------------------------------------------------------------


{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Node.Encoded where
    
import Data.Vector

-- | An encoded node allows getting and setting on encoded data
class EncodedNode n s | n -> s where
  getEncoded :: n -> Vector s
  setEncoded :: n -> Vector s -> n
