module PCG.Command.Types.Read.Unification.UnificationError where

import Data.List.NonEmpty
import Data.Semigroup

type TaxaName = String

data UnificationError
   = UnificationError (NonEmpty UnificationErrorMessage)
   deriving (Show)

data UnificationErrorMessage
   = NonMatchingTaxa     [TaxaName] [TaxaName] 
   | NonMatchingTaxaSeqs [TaxaName] [TaxaName]
   | ForestDuplicateTaxa   (NonEmpty TaxaName) FilePath
   | ForestExtraTaxa       (NonEmpty TaxaName) FilePath
   | ForestMissingTaxa     (NonEmpty TaxaName) FilePath

instance Show UnificationErrorMessage where
  show (NonMatchingTaxa xs ys) =
    concat [ "LHS: "
           , show xs
           , "\nRHS: "
           , show ys
           ]
  show (NonMatchingTaxaSeqs xs ys) =
    concat [ "LHS: "
           , show xs
           , "\nRHS:  "
           , show ys
           ]
  show (ForestDuplicateTaxa names path) =
    concat [ "The trees from file '"
           , path
           , "' contain an multiple entries for the following taxa: "
           , show $ toList names
           ,"."
           ]    
  show (ForestExtraTaxa names path) =
    concat [ "A tree from file '"
           , path
           , "' contains an entry for the following taxa not included in the data set(s): "
           , show $ toList names
           ,"."
           ]
  show (ForestMissingTaxa names path) =
    concat [ "None of the trees from file '"
           , path
           , "' contain an entry for the taxa: "
           , show $ toList names
           ,"."
           ]

instance Semigroup UnificationError where
    (UnificationError messages1) <> (UnificationError messages2) = UnificationError (messages1 <> messages2)
