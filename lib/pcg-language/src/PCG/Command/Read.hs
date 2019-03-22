-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"READ\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedSums      #-}

module PCG.Command.Read
  ( FileSpecification(..)
  , FileSpecificationContent(..)
  , ReadCommand(..)
  , FileContent
  , FileResult
  , DataContent(..)
  , TcmReference
  , readCommandSpecification
  ) where

import Control.Applicative.Free (Ap)
import Data.List.NonEmpty       (NonEmpty)
import Data.Text                (Text)
import PCG.Syntax.Combinators


-- |
-- The \"READ\" command containing the files paths to be read.
newtype ReadCommand = ReadCommand (NonEmpty FileSpecification)
    deriving (Show)


-- |
-- The collection of file content collected from a 'FileSpecification'.
newtype FileSpecificationContent = SpecContent (NonEmpty DataContent)


-- |
-- Content of a single data file along with a possibly associated TCM file content.
data  DataContent
    = DataContent
    { dataFile :: !FileResult
    , tcmFile  :: !(Maybe FileResult)
    } deriving (Eq)


-- |
-- The specification for a file to be read.
data  FileSpecification
    = UnspecifiedFile    !(NonEmpty FilePath) --Try to parse them all?
    | AminoAcidFile      !(NonEmpty FilePath)
    | NucleotideFile     !(NonEmpty FilePath)
    | AnnotatedFile      !(NonEmpty FilePath)
    | ChromosomeFile     !(NonEmpty FilePath)
    | GenomeFile         !(NonEmpty FilePath)
    | CustomAlphabetFile !(NonEmpty FilePath) !TcmReference
    | WithSpecifiedTCM   !FileSpecification   !TcmReference
    | PrealignedFile     !FileSpecification
    deriving (Show)


-- |
-- The content of a file.
type  FileContent  = Text


-- |
-- The context of reading a file along with the path the content originated from.
type  FileResult   = (FilePath, FileContent)


-- |
-- An optional reference to a TCM file.
type  TcmReference = FilePath


instance Semigroup ReadCommand where

    (ReadCommand lhs) <> (ReadCommand rhs) = ReadCommand $ lhs <> rhs


-- |
-- Defines the semantics of interpreting a valid \"READ\" command from the PCG
-- scripting language syntax.
readCommandSpecification :: CommandSpecification ReadCommand
readCommandSpecification = command "read" $ ReadCommand <$> someOf fileSpec


fileSpec :: Ap SyntacticArgument FileSpecification
fileSpec = choiceFrom
    [ unspecified
    , withSpecTCM
    , prealigned
    , customAlphabet
    , aminoAcids
    , nucleotides
    , annotated
    , chromosome
    , genome
    ]
  where
    unspecified    = UnspecifiedFile . pure <$> text
    aminoAcids     = AminoAcidFile  <$> oneOrSomeWithIds text [ "amino_acid", "amino_acids", "aminoacid", "aminoacids", "protein", "proteins" ]
    nucleotides    = NucleotideFile <$> oneOrSomeWithIds text [ "nucleotide", "nucleotides" ]
    annotated      = AnnotatedFile  <$> oneOrSomeWithIds text [ "annotated" ]
    chromosome     = ChromosomeFile <$> oneOrSomeWithIds text [ "chromosome", "chromosomes", "chromosomal" ]
    genome         = GenomeFile     <$> oneOrSomeWithIds text [ "genome", "genomes", "genomic", "genomics" ]
    prealigned     = argId "prealigned"      . singleArgList $ PrealignedFile <$> fileSpec
    customAlphabet = argId "custom_alphabet" . argList $ CustomAlphabetFile <$> oneOrSome text <*> tcmReference
    withSpecTCM    = argId "set_tcm" . argList $ WithSpecifiedTCM <$> fileSpec <*> tcmReference


    tcmReference :: Ap SyntacticArgument String
    tcmReference   = argId "tcm" text


-- |
-- Can either place parens around the argument or not.
singleArgList :: Ap SyntacticArgument a -> Ap SyntacticArgument a
singleArgList v = choiceFrom [ v, argList v ]


-- |
-- Makes file parsing even more flexible as a single file can be specified
-- without parens or many files may be specified with parens.
oneOrSome :: Ap SyntacticArgument a -> Ap SyntacticArgument (NonEmpty a)
oneOrSome v = choiceFrom [ pure <$> v, someOf v ]


-- |
-- Makes file parsing even more flexible as a single file can be specified
-- without parens or many files may be specified with parens.
oneOrSomeWithIds :: Foldable f => Ap SyntacticArgument a -> f String -> Ap SyntacticArgument (NonEmpty a)
oneOrSomeWithIds v strs = choiceFrom [pure <$> argIds strs v, argIds strs (someOf v)]
