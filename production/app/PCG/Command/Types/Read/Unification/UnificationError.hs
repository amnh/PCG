module PCG.Command.Types.Read.Unification.UnificationError where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup

type TaxaName = String

data UnificationError
   = UnificationError (NonEmpty UnificationErrorMessage)

data UnificationErrorMessage
   = NonMatchingTaxa     [TaxaName] [TaxaName] 
   | NonMatchingTaxaSeqs [TaxaName] [TaxaName]
   | ForestDuplicateTaxa (NonEmpty TaxaName) FilePath
   | ForestExtraTaxa     (NonEmpty TaxaName) FilePath
   | ForestMissingTaxa   (NonEmpty TaxaName) FilePath

instance Show UnificationError where
  show (UnificationError xs) = unlines $ show <$> toList xs

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
           , "' contain an multiple entries for the following taxa: \n"
           , listShow names
           ]    
  show (ForestExtraTaxa names path) =
    concat [ "A tree from file '"
           , path
           , "' contains an entry for the following taxa not included in the data set(s): \n"
           , listShow names
           ]
  show (ForestMissingTaxa names path) =
    concat [ "None of the trees from file '"
           , path
           , "' contain an entry for the taxa: \n"
           , listShow names
           ]

listShow :: (Foldable t, Show a) => t a -> String
listShow = (\x -> "[ " <> x <> "]") . drop 2 . unlines . fmap ((", " <>) . show) . toList

instance Semigroup UnificationError where
    (UnificationError messages1) <> (UnificationError messages2) = UnificationError (messages1 <> messages2)
