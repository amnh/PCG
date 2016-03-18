module PCG.Command.Types.Read.Unification.UnificationError where

import Data.List.NonEmpty

type TaxaName = String

data UnificationError = UnificationError (NonEmpty UnificationErrorMessage)

data UnificationErrorMessage = NonMatchingTaxa (NonEmpty TaxaName) (NonEmpty TaxaName)