----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unification.InputData
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the command for collecting all input types and checking consistency of:
--   * Topological
--   * Metadata
--   * Character sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Unification.InputData
  ( PartialInputData(..)
  , InputData(..)
  , collectPartialInputs
  ) where

import           Bio.Graph.Component
import           Bio.Graph.ReferenceDAG
import           Control.Arrow                ((&&&))
import           Control.Monad.State.Strict
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor               (first)
import           Data.Coerce                  (coerce)
import           Data.Foldable
import           Data.Functor
import           Data.List.NonEmpty           (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty           as NE
import           Data.List.Utility            (duplicates)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.NodeLabel
import           Data.Normalization.Character
import           Data.Normalization.Metadata
import           Data.Normalization.Topology
import           Data.Semigroup               ((<>))
import           Data.Semigroup.Foldable
import           Data.Set                     (Set, (\\))
import qualified Data.Set                     as Set
import           Data.String
import           Data.TCM                     (TCM, TCMStructure (..))
import           Data.Unification.Error
import           Data.Validation
import           Data.Vector.NonEmpty         (Vector)
import           Prelude                      hiding (lookup, zipWith)


-- |
-- A single component of the input data.
--
-- Requires unification with zero or more 'PartialInputData' values to create an
-- 'InputData' value that is consistent.
data  PartialInputData
     = PID
     { parsedChars   :: NormalizedCharacters
     , parsedMetas   :: Maybe (Vector NormalizedMetadata)
     , parsedForests :: NormalizedForestSet
     , relatedTcm    :: Maybe (TCM, TCMStructure)
     , sourceFile    :: FilePath
     }


-- |
-- A single, consistent collection of the input data.
data  InputData
    = InputData
    { dataSequences :: Maybe (NonEmpty PartialInputData)
    , taxaSet       :: Set Identifier
    , allForests    :: Maybe (NonEmpty PartialInputData)
    , forestTaxa    :: Maybe (NonEmpty ([NonEmpty Identifier], PartialInputData))
    } deriving (Show)


instance Show PartialInputData where

    show pid = unlines
        [ "PID"
        , "  { parsedChars   = " <> show (parsedChars pid)
        , "  , parsedMetas   = " <> show (parsedMetas pid)
        , "  , parsedForests = " <> if null (parsedForests pid) then "Nothing" else "Just <trees>"
        , "  , relatedTcm    = " <> show (relatedTcm  pid)
        , "  , sourceFile    = " <> show (sourceFile  pid)
        , "  }"
        ]


-- |
-- Convert a collection of partial inputs from disparate sources into a single
-- consistent input data bundle or report unification errors if the collection of
-- partial inputs was inconsistent.
collectPartialInputs :: Foldable1 f => f PartialInputData -> Validation UnificationError InputData
collectPartialInputs pids =
    unificationTransformation (gatherPartialInputData pids) `bindValidation` getUnificationErrors


gatherPartialInputData :: Foldable1 f => f PartialInputData -> InputData
gatherPartialInputData pids = InputData{..}
  where
    -- Gather data file contents
    dataSequences   = nonEmpty . filter (not . fromTreeOnlyFile) $ toList pids

    -- Union the taxa names together into total terminal set
    taxaSet         = foldMap (fold1 . ((Map.keysSet . parsedChars) `pmap`)) dataSequences

    -- Gather forest file data
    allForests      = nonEmpty . filter (not . null . parsedForests) $ toList pids

    -- Gather the taxa names for each forest from terminal nodes
    forestTaxa      = (gatherForestsTerminalNames `pmap`) <$> allForests


-- |
-- There is currently no input data transformation logic.
-- Here we could apply a filter to restrict the data set.
-- Or we could transform the taxa names, character values, etc.
unificationTransformation :: InputData -> Validation UnificationError InputData
unificationTransformation = Success


getUnificationErrors :: InputData -> Validation UnificationError InputData
getUnificationErrors v@InputData{..} = foldr1 (<~>) possibleUnificationErrors $> v
  where
    possibleUnificationErrors :: NonEmpty (Validation UnificationError ())
    possibleUnificationErrors = NE.fromList
        [ validateInputWith forestWithDuplicateTaxa duplicates
        , validateInputWith forestWithExtraTaxa     hasExtraNames
        , validateInputWith forestWithMissingTaxa   hasMissingNames
        ]

    -- Assert that each forest's terminal node set is not a proper superset of
    -- the taxa set from "data files"
    hasExtraNames   = (\\ taxaSet) . Set.fromList . toList

    -- Assert that each forest's terminal node set is not a proper subset of
    -- the taxa set from "data files"
    hasMissingNames = (taxaSet \\) . Set.fromList . toList

    validateInputWith
      :: Foldable f
      => (FilePath -> NonEmpty Identifier -> UnificationError)
      -> (NonEmpty Identifier -> f Identifier)
      -> Validation UnificationError ()
    validateInputWith c f =
        case expandForestErrors $ collectTaxaWith f of
          []   -> Success ()
          x:xs -> foldMap1 (colateErrors c) $ x:|xs

    collectTaxaWith
      :: Foldable f
      => (NonEmpty Identifier -> f a)
      -> [([f a], PartialInputData)]
    collectTaxaWith f = foldMap (NE.filter hasData . (first (fmap f) `pmap`)) forestTaxa
      where
        hasData :: (Foldable f, Foldable t) => (f (t a), b) -> Bool
        hasData = any (not . null) . fst


expandForestErrors :: [([t a], PartialInputData)] -> [[(t a, PartialInputData)]]
expandForestErrors = fmap f
  where
    f (ys, pid) = (id &&& const pid) <$> ys


colateErrors
  :: (Foldable t, Foldable t')
  => (FilePath -> NonEmpty Identifier -> UnificationError)
  -> t (t' Identifier, PartialInputData)
  -> Validation UnificationError ()
colateErrors f xs =
  case toList xs of
    []   -> Success ()
    y:ys -> Failure . foldMap1 transformPID $ y:|ys
  where
    transformPID (x,y) = f (sourceFile y) . NE.fromList $ toList x


fromTreeOnlyFile :: PartialInputData -> Bool
fromTreeOnlyFile pid = null chars || all null chars
  where
    chars = parsedChars pid


terminalNames2 :: ReferenceDAG a b (Maybe NodeLabel) -> [Identifier]
terminalNames2 dag = coerce . mapMaybe (`nodeDatum` dag) $ leaves  dag


gatherForestsTerminalNames :: PartialInputData -> ([NonEmpty Identifier], PartialInputData)
gatherForestsTerminalNames pid = (identifiers, pid)
  where
    identifiers :: [NonEmpty Identifier]
    identifiers =
        case parsedForests pid of
          Nothing      -> []
          Just (e:|es) -> catMaybes $ f <$> (e:es)
      where
        f forest =
          case foldMap terminalNames2 forest of
             []   -> Nothing
             x:xs -> Just (x :| xs)


-- |
-- Like (<>) except collect 'Failure' values instead of 'Success' values.
(<~>) :: Semigroup e => Validation e a -> Validation e b -> Validation e a
(<~>) (Failure x) (Failure y) = Failure $ x <> y
(<~>) (Failure x) (Success _) = Failure   x
(<~>) (Success _) (Failure y) = Failure   y
(<~>) (Success x) (Success _) = Success   x


-- |
-- Specify a parallel map.
pmap :: (a -> b) -> NonEmpty a -> NonEmpty b
pmap = parmap rpar
