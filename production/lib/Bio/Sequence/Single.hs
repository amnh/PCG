-- | A sequence that is not permitted to have ambiguity

module Bio.Sequence.Single where

-- | A single sequence may have no ambiguity, so it's capable of checking that condition
class SingleSequence s where
    checkAmbiguous :: s -> Bool