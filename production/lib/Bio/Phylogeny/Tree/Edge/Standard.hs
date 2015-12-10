{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Edge.Standard where

class StandardEdge e n | n -> e where
    edgeLen :: e -> Float
    setEdgeLen :: e -> Float -> e
    origin :: e -> n
    terminal :: e -> n
    connection :: e -> (n, n)

    connection edge = (origin edge, terminal edge)
    origin edge = fst $ connection edge
    terminal edge = snd $ connection edge
