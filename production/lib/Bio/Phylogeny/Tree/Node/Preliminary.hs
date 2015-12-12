{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Preliminary where

import Data.Vector

-- | A preliminary node has a preliminary assignment as well as associated data
-- Associated data: aligned preliminary, temporary, and cost
class PreliminaryNode a b | a -> b where
    preliminary      :: a -> Maybe (Vector (Vector b))
    setPreliminary   :: Maybe (Vector (Vector b)) -> a -> a
    preliminaryAlign :: a -> Maybe (Vector (Vector b))
    setAlign         :: Maybe (Vector (Vector b)) -> a -> a
    temporary        :: a -> Maybe (Vector (Vector b))
    setTemporary     :: Maybe (Vector (Vector b)) -> a -> a
    cost             :: a -> Float
    setCost          :: Float -> a -> a
