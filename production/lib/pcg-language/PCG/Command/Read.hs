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
-- Provides the types fot the Read command allong with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Read
  ( CustomAlphabetOptions(..)
  , CustomAlphabetStrategy(..)
  , FileSpecification(..)
  , FileSpecificationContent(..)
  , ReadCommand(..)
  , FileContent
  , FileResult
  , TcmReference
  , Tiebreaker(..)
  , readCommandSpecification
  ) where

import           Control.Applicative.Free (Ap)
import           Data.Foldable
import           Data.Functor             (($>))
import           Data.List.NonEmpty       (NonEmpty())
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup           (Semigroup(..))
import           Data.Text                (Text)
import           PCG.Syntax.Combinators


-- |
-- The Read command containing the files paths to be read.
newtype ReadCommand = ReadCommand (NonEmpty FileSpecification)
    deriving (Show)
      

-- |
-- The content of a file along with a possibly associated TCM file content.
data  FileSpecificationContent
    = SpecContent
    { dataFiles :: [FileResult]
    , tcmFile   :: Maybe FileResult
    } deriving (Eq)


-- |
-- The specification for a file to be read.
data  FileSpecification
    = UnspecifiedFile    (NonEmpty FilePath) --Try to parse them all?
    | AminoAcidFile      (NonEmpty FilePath)
    | NucleotideFile     (NonEmpty FilePath)
    | AnnotatedFile      (NonEmpty FilePath)
    | ChromosomeFile     (NonEmpty FilePath)
    | GenomeFile         (NonEmpty FilePath)
    | CustomAlphabetFile (NonEmpty FilePath) TcmReference [CustomAlphabetOptions]
    | PrealignedFile     FileSpecification TcmReference
    deriving (Show)


-- |
-- Options for custom alphabets. Not sure how these will be evaluation.
data  CustomAlphabetOptions
    = Init3D Bool
    | Level  Int  (Either CustomAlphabetStrategy Tiebreaker)
    | Ties   Tiebreaker
    deriving (Show)


-- |
-- Describes how ties are to be broken. In what context ties are occuring, I'm
-- not sure.
data Tiebreaker = Tiebreaker CustomAlphabetStrategy
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
-- Defines the semantics of interpreting a valid \"Read\" command from the PCG
-- scripting language syntax.
readCommandSpecification :: CommandSpecification ReadCommand
readCommandSpecification = command "read" $ ReadCommand <$> someOf fileSpec


fileSpec :: Ap SyntacticArgument FileSpecification
fileSpec = choiceFrom [ unspecified, customAlphabet, aminoAcids, nucleotides, annotated, chromosome, genome, prealigned  ]
  where
    unspecified    = UnspecifiedFile . pure <$> text
    aminoAcids     = AminoAcidFile  <$> oneOrSomeWithIds text [ "amino_acid", "amino_acids", "aminoacid", "aminoacids" ]
    nucleotides    = NucleotideFile <$> oneOrSomeWithIds text [ "nucleotide", "nucleotides" ]
    annotated      = AnnotatedFile  <$> oneOrSomeWithIds text [ "annotated" ]
    chromosome     = ChromosomeFile <$> oneOrSomeWithIds text [ "chromosome", "chromosomes", "chromosomal" ]
    genome         = GenomeFile     <$> oneOrSomeWithIds text [ "genome", "genomes", "genomic", "genomics" ]
    prealigned     = argId "prealigned"      . argList $ PrealignedFile <$> fileSpec <*> tcmReference
    customAlphabet = argId "custom_alphabet" . argList $ CustomAlphabetFile <$> fileRefs <*> tcmReference <*> alphabetOpts
      where
        fileRefs     = oneOrSome text
        alphabetOpts = (toList <$> oneOrSome alphabetOpt) `withDefault` []
        alphabetOpt  = choiceFrom [ initSpec, levelSpec, Ties <$> tiebreaker ]
          where
            initSpec   = Init3D     <$> argId "init3d" bool
            tiebreaker = Tiebreaker <$> argIds [ "tie_breaker", "tiebreaker" ] strategy
            levelSpec  = argId "level" . argList $ Level <$> int <*> choiceFrom [ Left <$> strategy, Right <$> tiebreaker]

            strategy  = choiceFrom
                [ value "first"     $> First
                , value "last"      $> Last
                , value "at_random" $> AtRandom
                , value "randomly"  $> AtRandom
                , value "random"    $> AtRandom
                ]

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
