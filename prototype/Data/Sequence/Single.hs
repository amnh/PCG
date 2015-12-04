-- | A sequence that is not permitted to have ambiguity

module Data.Sequence.Single where

class SingleSequence s where
    checkAmbiguous :: s -> Bool