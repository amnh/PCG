----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unification.Error
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Create and merge errors that occur during data unificaation.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.Unification.Error
  ( UnificationError()
  -- * Constructors
  , forestWithDuplicateTaxa
  , forestWithExtraTaxa
  , forestWithMissingTaxa
  , vacuousInputFiles
  ) where

import Data.Foldable
import Data.List.NonEmpty     (NonEmpty)
import Data.Text.Short        hiding (toString)
import Data.Text.Short.Custom ()
import TextShow
import TextShow.Custom


-- |
-- A collection of errors that occured during unification.
--
-- Has nice 'Show'/'TextShow' instances for rendering.
newtype UnificationError
      = UnificationError (NonEmpty UnificationErrorMessage)


data  UnificationErrorMessage
    = NonMatchingTaxa     [ShortText] [ShortText]
    | NonMatchingTaxaSeqs [ShortText] [ShortText]
    | ForestDuplicateTaxa FilePath (NonEmpty ShortText)
    | ForestExtraTaxa     FilePath (NonEmpty ShortText)
    | ForestMissingTaxa   FilePath (NonEmpty ShortText)
    | VacuousInput        (NonEmpty FilePath)
-- TODO: Add an error case for a nonempty set of taxa with only missing data observations.


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


-- |
-- Creates a UnificationError describing a forest supplied by an input file that
-- contains multiple, identical leaf labels.
forestWithDuplicateTaxa :: FilePath -> NonEmpty ShortText -> UnificationError
forestWithDuplicateTaxa path = UnificationError . pure . ForestDuplicateTaxa path


-- |
-- Creates a UnificationError describing a forest supplied by an input file that
-- contains one or more leaf labels that were not present in any data files.
forestWithExtraTaxa :: FilePath -> NonEmpty ShortText -> UnificationError
forestWithExtraTaxa path = UnificationError . pure . ForestExtraTaxa path


-- |
-- Creates a UnificationError describing a forest supplied by an input file that
-- has one or more leaf labels missing that were present in the data files.
forestWithMissingTaxa :: FilePath -> NonEmpty ShortText -> UnificationError
forestWithMissingTaxa path = UnificationError . pure . ForestMissingTaxa path


-- |
-- Creates a UnificationError describing one or more input fileswhere there were
-- niether any character sequences nor any trees found in the aforementioned
-- input files.
vacuousInputFiles :: NonEmpty FilePath -> UnificationError
vacuousInputFiles = UnificationError . pure . VacuousInput


listShowB :: (Foldable t, TextShow a) => t a -> Builder
listShowB v =
  case toList v of
    []   -> "[]"
    x:xs -> (\a -> "[ " <> a <> "]") . unlinesB
          -- Add a leading comma to everything except the first element
          -- Make everything Text values.
          $ showb x : ((", " <>) . showb <$> xs)

