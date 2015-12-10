module Data.Tree.Parsed where

class ParsedTree t o | o -> t where
    encode :: t -> o
    decode :: o -> t