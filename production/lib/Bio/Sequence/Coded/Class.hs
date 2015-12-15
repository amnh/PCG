{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Sequence.Coded.Class where

class Monoid s => CodedSequence s b where
    numChars :: s -> Int
    gapChar :: s
    grabChar :: s -> Int -> b
