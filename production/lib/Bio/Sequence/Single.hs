-- | A sequence that is not permitted to have ambiguity

module Bio.Sequence.Single where

class SingleSequence s where
    checkAmbiguous :: s -> Bool