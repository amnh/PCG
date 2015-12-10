module Bio.Phylogeny.Tree.Parsed where

class ParsedTree t o | o -> t where
    encode :: t -> o
    decode :: o -> t