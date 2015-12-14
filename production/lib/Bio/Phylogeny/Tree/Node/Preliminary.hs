{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Preliminary where

import Data.Vector
import Bio.Sequence.Coded

-- | A preliminary node has a preliminary assignment as well as associated data
-- Associated data: aligned preliminary, temporary, and cost
class PreliminaryNode n s | n -> s where
    preliminary      :: n -> Vector s
    setPreliminary   ::Vector s -> n -> n
    preliminaryAlign :: n ->Vector s
    setAlign         ::Vector s -> n -> n
    temporary        :: n ->Vector s
    setTemporary     ::Vector s -> n -> n
    cost             :: n -> Float
    setCost          :: Float -> n -> n
