{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Encoded where

import Bio.Sequence.Coded
import Data.Vector

-- | An encoded node allows getting and setting on encoded data
class EncodedNode n s | n -> s where
  encoded :: n -> Vector s
  setEncoded :: n -> Vector s -> n
