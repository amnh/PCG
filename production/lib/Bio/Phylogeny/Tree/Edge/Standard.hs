{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Edge.Standard where

-- | A standard edge allows you to get and set length as well as get the origin and terminal
class StandardEdge e n | n -> e where
    edgeLen :: e -> Float
    setEdgeLen :: e -> Float -> e
    origin :: e -> n
    terminal :: e -> n
    connection :: e -> (n, n)

    connection edge = (origin edge, terminal edge)
    origin edge = fst $ connection edge
    terminal edge = snd $ connection edge
