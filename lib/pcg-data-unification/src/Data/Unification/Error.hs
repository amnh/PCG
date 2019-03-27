{-# LANGUAGE OverloadedStrings #-}

module Data.Unification.Error
  ( UnificationError(..)
  , UnificationErrorMessage(ForestDuplicateTaxa, ForestExtraTaxa, ForestMissingTaxa, VacuousInput)
  ) where

import Data.Foldable
import Data.List.NonEmpty     (NonEmpty)
import Data.Text.Short        hiding (toString)
import Data.Text.Short.Custom ()
import TextShow
import TextShow.Custom


type TaxaName = ShortText


newtype UnificationError
      = UnificationError (NonEmpty UnificationErrorMessage)


-- TODO: Rename this better
data UnificationErrorMessage
   = NonMatchingTaxa     [TaxaName] [TaxaName]
   | NonMatchingTaxaSeqs [TaxaName] [TaxaName]
   | ForestDuplicateTaxa (NonEmpty TaxaName) FilePath
   | ForestExtraTaxa     (NonEmpty TaxaName) FilePath
   | ForestMissingTaxa   (NonEmpty TaxaName) FilePath
   | VacuousInput        (NonEmpty FilePath)
-- TODO: Add an error case for a nonempty set oft axa with only missing data observations.


instance Semigroup UnificationError where

    (UnificationError messages1) <> (UnificationError messages2) = UnificationError (messages1 <> messages2)


instance Show UnificationError where

    show (UnificationError xs) = unlines $ show <$> toList xs


instance Show UnificationErrorMessage where

    show = toString . showb


instance TextShow UnificationErrorMessage where

    showb (NonMatchingTaxa xs ys) = fold
        [ "LHS: "
        , showb xs
        , "\nRHS: "
        , showb ys
        ]

    showb (NonMatchingTaxaSeqs xs ys) = fold
        [ "LHS: "
        , showb xs
        , "\nRHS:  "
        , showb ys
        ]

    showb (ForestDuplicateTaxa names path) = fold
        [ "The trees from file '"
        , showb path
        , "' contain an multiple entries for the following taxa: \n"
        , listShowB names
        ]

    showb (ForestExtraTaxa names path) = fold
        [ "A tree from file '"
        , showb path
        , "' contains an entry for the following taxa not included in the data set(s): \n"
        , listShowB names
        ]

    showb (ForestMissingTaxa names path) = fold
        [ "None of the trees from file '"
        , showb path
        , "' contain an entry for the taxa: \n"
        , listShowB names
        ]

    showb (VacuousInput files) = fold
       [ "There was niether any character sequences nor any trees found in any of the supplied input files:\n"
       , (\x -> "  ["<>x<>"]") . intercalateB ", " $ showb <$> toList files
       ]


listShowB :: (Foldable t, TextShow a) => t a -> Builder
listShowB v =
  case toList v of
    []   -> "[]"
    x:xs -> (\a -> "[ " <> a <> "]") . unlinesB
          -- Add a leading comma to everything except the first element
          -- Make everything Text values.
          $ showb x : ((", " <>) . showb <$> xs)

