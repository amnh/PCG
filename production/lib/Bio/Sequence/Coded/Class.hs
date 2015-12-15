{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

class Monoid s => CodedSequence s b | s -> b where
    numChars :: s -> Int
    emptySeq :: s
    isEmpty :: s -> Bool
    grabSubChar :: s -> Int -> Maybe b
    filterSeq :: s -> (b -> Bool) -> s
    charToSeq :: b -> s

class CodedChar b where
    gapChar :: b
