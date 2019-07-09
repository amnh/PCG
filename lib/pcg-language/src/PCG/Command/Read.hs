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
  , ReadCommand(..)
  , TcmReference
  , readCommandSpecification
  ) where

import Control.Applicative.Free (Ap)
import Data.FileSource
import Data.List.NonEmpty       (NonEmpty)
import PCG.Syntax.Combinators


-- |
-- The \"READ\" command containing the files paths to be read.
newtype ReadCommand = ReadCommand (NonEmpty FileSpecification)
    deriving (Show)


-- |
-- The specification for a file to be read.
data  FileSpecification
    = UnspecifiedFile    !(NonEmpty FileSource) --Try to parse them all?
    | AminoAcidFile      !(NonEmpty FileSource)
    | NucleotideFile     !(NonEmpty FileSource)
    | AnnotatedFile      !(NonEmpty FileSource)
    | ChromosomeFile     !(NonEmpty FileSource)
    | GenomeFile         !(NonEmpty FileSource)
    | CustomAlphabetFile !(NonEmpty FileSource) !TcmReference
    | WithSpecifiedTCM   !FileSpecification   !TcmReference
    | PrealignedFile     !FileSpecification
    deriving (Show)


-- |
-- An optional reference to a TCM file.
type  TcmReference = FileSource


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
    unspecified    = UnspecifiedFile . pure <$> filePath
    aminoAcids     = AminoAcidFile  <$> oneOrSomeWithIds filePath [ "amino_acid", "amino_acids", "aminoacid", "aminoacids", "protein", "proteins" ]
    nucleotides    = NucleotideFile <$> oneOrSomeWithIds filePath [ "nucleotide", "nucleotides" ]
    annotated      = AnnotatedFile  <$> oneOrSomeWithIds filePath [ "annotated" ]
    chromosome     = ChromosomeFile <$> oneOrSomeWithIds filePath [ "chromosome", "chromosomes", "chromosomal" ]
    genome         = GenomeFile     <$> oneOrSomeWithIds filePath [ "genome", "genomes", "genomic", "genomics" ]
    prealigned     = argId "prealigned"      . singleArgList $ PrealignedFile <$> fileSpec
    customAlphabet = argId "custom_alphabet" . argList $ CustomAlphabetFile <$> oneOrSome filePath <*> tcmReference
    withSpecTCM    = argId "set_tcm" . argList $ WithSpecifiedTCM <$> fileSpec <*> tcmReference


    tcmReference :: Ap SyntacticArgument FileSource
    tcmReference   = argId "tcm" filePath


-- |
-- Definition of a 'FileSource' in the free context.
filePath :: Ap SyntacticArgument FileSource
filePath = FileSource <$> text


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
