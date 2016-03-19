module PCG.Command.Types.Read.Unification.UnificationError where

import Data.List.NonEmpty
import Data.Semigroup

type TaxaName = String

data UnificationError = UnificationError (NonEmpty UnificationErrorMessage)

data UnificationErrorMessage = NonMatchingTaxa (NonEmpty TaxaName) (NonEmpty TaxaName)

instance Semigroup UnificationError where
    (UnificationError messages1) <> (UnificationError messages2) = UnificationError (messages1 <> messages2)