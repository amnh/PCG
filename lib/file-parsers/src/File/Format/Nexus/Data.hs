-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Nexus.Data
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Datatypes necessary for parsing Nexus format files.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}

module File.Format.Nexus.Data
  ( Sequences
  , AlphabetSymbol
  , AmbiguityGroup
  , TaxonIdentifier
  , CharacterMetadata(..)
  , Character
  , Sequence
  , TaxonSequenceMap
  , AssumptionBlock(..)
  , AssumptionField(..)
  , CharFormatField(..)
--  , CharStateFormat(..)
  , CharacterFormat(..)
  , CharDataType(..)
  , DimensionsFormat(..)
  , IgnBlock(..)
  , Nexus(..)
  , NexusBlock(..)
  , NexusParseResult(..)
  , PhyloSequence(..)
  , SeqSubBlock(Matrix, Format, Dims, Eliminate, CharLabels, IgnSSB, Taxa)
  , StepMatrix(..)
  , TaxaSpecification(..)
  , TreeName
--  , SerializedTree
  , TreeBlock(..)
  , TreeField(..)
--  , SequenceBlock(..)
  ) where

import qualified Data.Map.Lazy                    as M
import qualified Data.Vector                      as V
import           File.Format.Newick
import           File.Format.TransitionCostMatrix



--------------------------------------------------------------
----------------- Types for sending data out -----------------
--------------------------------------------------------------


-- |
-- Mapping of taxa names to character data and a vector corresponding to each
-- character's metadata.
type Sequences = ( TaxonSequenceMap
                 , V.Vector CharacterMetadata
                 )


-- |
-- Represents a symbol in the character alphabet.
type AlphabetSymbol = String


-- TODO: Use Data.List.NonEmpty here?
-- |
-- Represents an ambiguity group of symbols in the character alphabet.
-- Should always be a non empty list.
type AmbiguityGroup = [AlphabetSymbol]


-- |
-- Represents the unique identifier of a taxon.
type TaxonIdentifier = String


-- |
-- Metadata record type for a given character.
data  CharacterMetadata
    = CharacterMetadata
    { name      :: String
    , isAligned :: Bool
    , charType  :: CharDataType
    , alphabet  :: AmbiguityGroup
    , ignored   :: Bool
    , costM     :: Maybe TCM
    , additive  :: Bool
    , weight    :: Int
    }
    deriving stock (Show)


-- |
-- Character is Maybe, because some chars might not be present for some taxa
-- if unaligned, multiple characters are treated as a single "character", so V.length >= 1
-- if aligned, each character is a "character", so V.length == 1
type Character = Maybe (V.Vector AmbiguityGroup)


-- |
-- This is a Vector, because Character may have length /= 1
-- (see explanation at Character)
type Sequence = V.Vector Character


-- |
-- Mapping of taxon identifiers (typically names) to thier corresponding
-- sequence data.
type TaxonSequenceMap = M.Map TaxonIdentifier Sequence


--------------------------------------------------------------
-------------- Types for parsing and validation --------------
--------------------------------------------------------------

-- |
-- AssumptionBlock is a spec'd block in Nexus format. We're only interested in a single entity in this block
-- for now, the step matrix, but this datatype is included for later extensibility
data  AssumptionBlock
    = AssumptionBlock
    { tcm :: [StepMatrix]
    , add :: [Bool]
    }
    deriving stock (Show)


-- | AssumptionField is a list of fields in the Assumptions block.
data  AssumptionField
    = TCMMat StepMatrix
    | Add    Bool
    | IgnAF  String


-- |
-- The different subfields of the Format field in the sequence blocks.
-- As with SeqSubBlock, listed simply so that it can be "looped" over. Will eventually be
-- coverted to CharacterFormat data type for export
-- TODO: better documentation on the use of each field below
data  CharFormatField
    = CharDT      CharDataType
    | SymStr      (Either String [String]) -- the list of symbols
    | EqStr       (Either String [String]) -- the equate (symbol -> symbols) will be processed into Map Char String
    | MissStr     String -- "missing" char
    | GapChar     String -- gap char
    | MatchChar   String -- "match" char
    | Items       String
    | RespectCase Bool   -- should the case of the characters be respected?
    | AreTokens   Bool
    | Transpose   Bool
    | Interleave  Bool
    | Unlabeled   Bool   -- if seqMatrix is unlabeled, in which case first token in each line is a char
    | IgnFF       String -- for non-standard inputs, plus notokens, which is the default anyway
    deriving stock (Eq, Show)


-- | CharStateFormat is currently unused, but may be in future. Included here for completeness.
data  CharStateFormat
    = CharStateFormat
    { charNum   :: Int
    , charName  :: String
    , stateName :: [String]
    }
    deriving stock (Show)


-- |
-- CharacterFormat
-- Note that symbols may or may not be space-delimited. I work under the assumption that it is iff.
-- Tokens is spec'd, as well.
data  CharacterFormat
    = CharacterFormat
    { charDataType :: CharDataType    -- see CharDataType for options
    , symbols      :: Either String [String]
    , equate       :: Either String [String]
    , missing      :: String
    , gap          :: String
    , matchChar    :: String
    , items        :: String
    , respectCase  :: Bool
    , areTokens    :: Bool
    , transpose    :: Bool
    , interleave   :: Bool
    , unlabeled    :: Bool
    }
    deriving stock (Eq, Show)


-- |
-- The types of data which can be present in a Nexus file.
-- This type might get scrapped and another type imported from
-- different module, or preserved but moved to another module.
data  CharDataType
    = Standard
    | DNA
    | RNA
    | Nucleotide
    | Protein
    | Continuous
    deriving stock (Eq, Read, Show)


-- |
-- DimensionsFormat is format of dimensions field in characters, data and unaligned nexus blocks.
-- It could also actually be used for the dimensions in the taxa block, with newTaxa = False
-- and numChars = 0
data  DimensionsFormat
    = DimensionsFormat
    { newTaxa  :: Bool
    , numTaxa  :: Int
    , numChars :: Int
    }
    deriving stock (Show)


-- |
-- Where any information that should be ignored is stored.
newtype IgnBlock = IgnBlock { ignoredName :: String }
    deriving stock (Show)


-- |
-- The collection of information extracted from blocks in the Nexus file.
data  Nexus
    = Nexus
    -- TODO: taxa was commented out before first push to Grace
    { {- taxa :: [TaxonIdentifier]
    ,-} sequences  :: Sequences
    {- , stepMatrices :: AssumptionBlock -}
    , nexusForests :: [NewickForest]
    }
    deriving stock (Show)


-- |
-- Types blocks in the Nexus file and their accompanying data.
data  NexusBlock
    = TaxaBlock        TaxaSpecification
    | CharacterBlock   PhyloSequence
    | TreesBlock       TreeBlock
    | SkippedBlock     IgnBlock
    | AssumptionsBlock AssumptionBlock
    deriving stock(Show)


-- |
--The output type of the Nexus parser, containing the results of each block in
-- the file, sorted by type.
data  NexusParseResult
    = NexusParseResult
    { pSeqs   :: [PhyloSequence]
    , spec    :: [TaxaSpecification]
    , treeB   :: [TreeBlock]
    , assumpB :: [AssumptionBlock]
    , ign     :: [IgnBlock]
    }
    deriving stock (Show)


-- | Phylosequence is general sequence type, to be used for both characters and data blocks (aligned) and unaligned blocks.
data  PhyloSequence
    = PhyloSequence
    { alignedSeq    :: Bool
    , seqMatrix     :: [[String]]
    , format        :: [CharacterFormat]
    , charDims      :: [DimensionsFormat] -- holds length of sequence, as well as info on new taxa
    , elims         :: [String]
    , seqTaxaLabels :: [[String]]
    , charLabels    :: [[String]]
    , blockType     :: String -- characters, taxa or data
    }
    deriving stock (Show)


-- |
-- SeqSubBlock is list of fields available in sequence blocks. It's set up as an enumeration
-- so that it can be "looped" over when parsing the sequence blocks, as the order of the fields
-- is not documented.
data  SeqSubBlock
    = Matrix          [String]
    | Format          CharacterFormat
    | Dims            DimensionsFormat
--    | Items           ItemType
    | Eliminate       String
    | CharStateLabels [CharStateFormat]
    | CharLabels      [String]
    | IgnSSB          String
    | Taxa            [String]
    deriving stock (Show)


-- | StepMatrix is type to be pulled from Assumptions block.
data  StepMatrix
    = StepMatrix
    { matrixType :: String -- Actually, the name of the tcm. Originally was to be step or real. Collected, but unused.
    , matrixSize :: Int    -- for validation
    , matrixData :: TCM
    }
    deriving stock (Show)


-- |
-- TaxaSpecification holds the number of taxa and the list of taxa names. It's collected from a taxa block, and appears as a subtype
-- of NexusBlock. Used for verification.
data  TaxaSpecification
    = TaxaSpecification
    { taxaDims   :: Int
    , taxaLabels :: [String]
    }
    deriving stock (Show)


-- | (✓)
type TreeName       = String


{-
-- | (✓)
type SerializedTree = String
-}


-- | (✓)
data  TreeBlock
    = TreeBlock
    { translate :: [[String]]
    , trees     :: [(TreeName, NewickNode)]
    }
    deriving stock (Show)


-- | (✓)
data  TreeField
    = Translation [String]
    | Tree        (TreeName, NewickNode)
    | IgnTF       String
    deriving stock (Show)


-- | (✓)
data  SequenceBlock
    = SequenceBlock
    { meta     :: CharacterMetadata
    , elimList :: String
    , seqs     :: [V.Vector (String, [String])]
    }
    deriving stock (Show)


{-
standardAlphabet :: [String]
standardAlphabet = ["0","1"]


dna :: M.Map String [String]
dna = M.fromList [ ("A",["A"])
                 , ("C",["C"])
                 , ("G",["G"])
                 , ("T",["T"])
                 , ("R",["A","G"])
                 , ("Y",["C","T"])
                 , ("M",["A","C"])
                 , ("K",["G","T"])
                 , ("S",["C","G"])
                 , ("W",["A","T"])
                 , ("H",["A","C","T"])
                 , ("B",["C","G","T"])
                 , ("V",["A","C","G"])
                 , ("D",["A","G","T"])
                 , ("N",["A","C","G","T"])
                 , ("X",["A","C","G","T"])
                 ]


dnaAlphabet :: [String]
dnaAlphabet = M.keys dna


rna :: M.Map String [String]
rna = M.fromList [ ("A",["A"])
                 , ("C",["C"])
                 , ("G",["G"])
                 , ("U",["U"])
                 , ("R",["A","G"])
                 , ("Y",["C","U"])
                 , ("M",["A","C"])
                 , ("K",["G","U"])
                 , ("S",["C","G"])
                 , ("W",["A","U"])
                 , ("H",["A","C","U"])
                 , ("B",["C","G","U"])
                 , ("V",["A","C","G"])
                 , ("D",["A","G","U"])
                 , ("N",["A","C","G","U"])
                 , ("X",["A","C","G","U"])
                 ]

rnaAlphabet :: [String]
rnaAlphabet = M.keys rna


nucleotide :: M.Map String [String]
nucleotide = M.insert "U" ["T"] dna


nucleotideAlphabet :: [String]
nucleotideAlphabet = "U" : dnaAlphabet


protein :: M.Map String [String]
protein = M.fromList [ ("A",["A"])
                     , ("C",["C"])
                     , ("D",["D"])
                     , ("E",["E"])
                     , ("F",["F"])
                     , ("G",["G"])
                     , ("H",["H"])
                     , ("I",["I"])
                     , ("K",["K"])
                     , ("L",["L"])
                     , ("M",["M"])
                     , ("N",["N"])
                     , ("P",["P"])
                     , ("Q",["Q"])
                     , ("R",["R"])
                     , ("S",["S"])
                     , ("T",["T"])
                     , ("V",["V"])
                     , ("W",["W"])
                     , ("Y",["Y"])
                     , ("*",["*"])
                     , ("B",["D","N"])
                     , ("Z",["E","Q"])
                     ]

proteinAlphabet :: [String]
proteinAlphabet = M.keys protein
-}
