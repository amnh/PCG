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
  , TcmReference
  , Tiebreaker(..)
  , readCommandSpecification
  ) where

import Control.Applicative.Free (Ap)
import Data.Foldable
import Data.Functor             (($>))
import Data.List.NonEmpty       (NonEmpty)
import Data.Text                (Text)
import PCG.Syntax.Combinators


-- |
-- The \"READ\" command containing the files paths to be read.
newtype ReadCommand = ReadCommand (NonEmpty FileSpecification)
    deriving (Show)


-- |
-- The content of a file along with a possibly associated TCM file content.
newtype FileSpecificationContent = SpecContent !(NonEmpty DataContent)


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
    | CustomAlphabetFile !(NonEmpty FileSpecification) !TcmReference
    | PrealignedFile     !(NonEmpty FileSpecification) !TcmReference
    deriving (Show)


-- |
-- Describes how ties are to be broken. In what context ties are occuring, I'm
-- not sure.
newtype Tiebreaker = Tiebreaker CustomAlphabetStrategy
    deriving (Show)


-- |
-- Strategy for alphabet symbols.
data  CustomAlphabetStrategy
    = First
    | Last
    | AtRandom
    deriving (Show)


-- |
-- The content of a file.
type  FileContent  = Text


-- |
-- The context of reading a file along with the path the content originated from.
type  FileResult   = (FilePath, FileContent)


-- |
-- An optional reference to a TCM file.
type  TcmReference = Maybe FilePath


instance Semigroup ReadCommand where

    (ReadCommand lhs) <> (ReadCommand rhs) = ReadCommand $ lhs <> rhs


-- |
-- Defines the semantics of interpreting a valid \"READ\" command from the PCG
-- scripting language syntax.
readCommandSpecification :: CommandSpecification ReadCommand
readCommandSpecification = command "read" $ ReadCommand <$> someOf fileSpec


fileSpec :: Ap SyntacticArgument FileSpecification
fileSpec = choiceFrom [ unspecified, customAlphabet, aminoAcids, nucleotides, annotated, chromosome, genome, prealigned ]
  where
    unspecified    = UnspecifiedFile . pure <$> text
    aminoAcids     = AminoAcidFile  <$> oneOrSomeWithIds text [ "amino_acid", "amino_acids", "aminoacid", "aminoacids", "protein", "proteins" ]
    nucleotides    = NucleotideFile <$> oneOrSomeWithIds text [ "nucleotide", "nucleotides" ]
    annotated      = AnnotatedFile  <$> oneOrSomeWithIds text [ "annotated" ]
    chromosome     = ChromosomeFile <$> oneOrSomeWithIds text [ "chromosome", "chromosomes", "chromosomal" ]
    genome         = GenomeFile     <$> oneOrSomeWithIds text [ "genome", "genomes", "genomic", "genomics" ]
    prealigned     = argId "prealigned"      . argList $ PrealignedFile     <$> oneOrSome fileSpec <*> tcmReference
    customAlphabet = argId "custom_alphabet" . argList $ CustomAlphabetFile <$> oneOrSome fileSpec <*> tcmReference
    tcmReference   = (Just <$> argId "tcm" (argList text)) `withDefault` Nothing


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
