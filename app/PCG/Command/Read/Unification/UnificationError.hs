module PCG.Command.Read.Unification.UnificationError where

import Data.Foldable
import Data.List          (intercalate)
import Data.List.NonEmpty (NonEmpty)


type TaxaName = String


newtype UnificationError
      = UnificationError (NonEmpty UnificationErrorMessage)


data UnificationErrorMessage
   = NonMatchingTaxa     [TaxaName] [TaxaName] 
   | NonMatchingTaxaSeqs [TaxaName] [TaxaName]
   | ForestDuplicateTaxa (NonEmpty TaxaName) FilePath
   | ForestExtraTaxa     (NonEmpty TaxaName) FilePath
   | ForestMissingTaxa   (NonEmpty TaxaName) FilePath
   | VacuousInput        (NonEmpty FilePath)


instance Semigroup UnificationError where
    (UnificationError messages1) <> (UnificationError messages2) = UnificationError (messages1 <> messages2)


instance Show UnificationError where
    show (UnificationError xs) = unlines $ show <$> toList xs


instance Show UnificationErrorMessage where
    show (NonMatchingTaxa xs ys) = mconcat
        [ "LHS: "
        , show xs
        , "\nRHS: "
        , show ys
        ]
    show (NonMatchingTaxaSeqs xs ys) = mconcat
        [ "LHS: "
        , show xs
        , "\nRHS:  "
        , show ys
        ]
    show (ForestDuplicateTaxa names path) = mconcat
        [ "The trees from file '"
        , path
        , "' contain an multiple entries for the following taxa: \n"
        , listShow names
        ]    
    show (ForestExtraTaxa names path) = mconcat
        [ "A tree from file '"
        , path
        , "' contains an entry for the following taxa not included in the data set(s): \n"
        , listShow names
        ]
    show (ForestMissingTaxa names path) = mconcat
        [ "None of the trees from file '"
        , path
        , "' contain an entry for the taxa: \n"
        , listShow names
        ]
    show (VacuousInput files) = mconcat
       [ "There was niether any character sequences nor any trees found in any of the supplied files input files:\n"
       , (\x -> "  ["<>x<>"]") . intercalate ", " $ show <$> toList files 
       ]

listShow :: (Foldable t, Show a) => t a -> String
listShow = (\x -> "[ " <> x <> "]") . drop 2 . unlines . fmap ((", " <>) . show) . toList

