{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

class Monoid s => CodedSequence s b | s -> b where
    numChars :: s -> Int
    gapChar :: s
    emptySeq :: s
    isEmpty :: s -> Bool
    grabSubChar :: s -> Int -> Maybe b

