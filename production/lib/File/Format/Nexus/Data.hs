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

module File.Format.Nexus.Data where

import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import           File.Format.Newick
import           File.Format.TransitionCostMatrix.Parser hiding (symbol)



--------------------------------------------------------------
----------------- Types for sending data out -----------------
--------------------------------------------------------------

type Sequences = [([TaxonIdentifier], TaxonSequenceMap, CharacterMetadata)]

type AlphabetSymbol = String

type AmbiguityGroup = [AlphabetSymbol]

type TaxonIdentifier = String

data CharacterMetadata 
   = CharacterMetadata
   { isAligned :: Bool
   , charType  :: CharDataType
   , alphabet  :: [AlphabetSymbol]
   , seqLength    :: Maybe Int
   , ignored   :: Bool  -- This is a problem, as input may interleave ignored and non-ignored chars, so given seqs will need to bbroken up
                       -- into separate seqs during validation (also true for TNT?).
                       -- That means that order of items in Sequences is important for IA output, I think. Maybe check with Ward?
   } deriving (Show)

type Sequence = V.Vector AmbiguityGroup

type TaxonSequenceMap = M.Map TaxonIdentifier Sequence

--type Sequences = [([TaxonIdentifier], TaxonSequenceMap, CharacterMetadata)]

---------------------------------------------------------------
--------------- Types for parsing and validation --------------
---------------------------------------------------------------

-- | AssumptionBlock is a spec'd block in Nexus format. We're only interested in a single entity in this block
-- for now, the step matrix, but this datatype is included for later extensibility
data AssumptionBlock
   = AssumptionBlock
   { tcm :: [StepMatrix] } deriving (Show)

-- | AssumptionField is a list of fields in the Assumptions block.
data AssumptionField
   = TCMMat StepMatrix
   | IgnAF  String

-- | The different subfields of the Format field in the sequence blocks.
-- As with SeqSubBlock, listed simply so that it can be "looped" over. Will eventually be
-- coverted to CharacterFormat data type for export
-- TODO: better documentation on the use of each field below
data CharFormatField
   = CharDT      String
   | SymStr      (Either String [String]) -- the list of symbols
   | EqStr       (Either String [String]) -- the equate (symbol -> symbols) will be processed into Map Char String
   | MissStr     String -- "missing" char
   | GapChar     String -- gap char
   | MatchChar   String -- "match" char
   | Items       String
   | RespectCase Bool   -- should the case of the characters be respected?
   | Tokens      Bool
   | Transpose   Bool
   | Interleave  Bool
   | Unlabeled   Bool   -- if seqMatrix is unlabeled, in which case first token in each line is a char
   | IgnFF       String -- for non-standard inputs, plus notokens, which is the default anyway
   deriving (Eq,Show)

-- | CharStateFormat is currently unused, but may be in future. Included here for completeness.
data CharStateFormat
   = CharStateFormat
   { charNum   :: Int
   , charName  :: String
   , stateName :: [String]
   } deriving (Show)

-- | CharacterFormat 
data CharacterFormat
   = CharacterFormat
   { charDataType :: String
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
   } deriving (Eq,Show)

-- | The types of data which can be present in a Nexus file.
-- This type might get scrapped and another type imported from
-- different module, or preserved but moved to another module.
data CharDataType = Standard | DNA | RNA | Nucleotide | Protein | Continuous deriving (Read, Show)

-- | DimensionsFormat is format of dimensions field in characters and unaligned nexus blocks.
-- It could also actually be used for the dimensions in the taxa block, with newTaxa = False
-- and numChars = 0
data DimensionsFormat
   = DimensionsFormat
   { newTaxa  :: Bool
   , numTaxa  :: Int
   , numChars :: Int
   } deriving (Show)

data IgnBlock = IgnBlock {ignoredName :: String} deriving (Show)

-- | The collection of information extracted from blocks in the Nexus file.
data Nexus
   = Nexus
   -- TODO: taxa was commented out before first push to Grace
   { {- taxa :: [String]
   ,-} characters :: M.Map String (V.Vector [String])
   , stepMatrices :: AssumptionBlock
   } deriving (Show)

-- | Types blocks in the Nexus file and their accompanying data.
data NexusBlock
   = TaxaBlock        TaxaSpecification
   | CharacterBlock   PhyloSequence
   | TreesBlock       TreeBlock
   | SkippedBlock     IgnBlock
   | AssumptionsBlock AssumptionBlock
   deriving (Show)

data NexusParseResult = NexusParseResult [PhyloSequence] [TaxaSpecification] [TreeBlock] [AssumptionBlock] [IgnBlock] deriving (Show)

-- | Phylosequence is general sequence type, to be used for both characters and data blocks (aligned) and unaligned blocks.
data PhyloSequence
   = PhyloSequence
   { alignedSeq    :: Bool
   , seqMatrix     :: [[String]]
   , format        :: [CharacterFormat]
   , charDims      :: [DimensionsFormat] -- holds length of sequence, as well as info on new taxa
   , elims         :: [String]
   , seqTaxaLabels :: [[String]]
   , name          :: String -- characters, taxa or data
   } deriving (Show)

-- | SeqSubBlock is list of fields available in sequence blocks. It's set up as an enumeration
-- so that it can be "looped" over when parsing the sequence blocks, as the order of the fields
-- is not documented.
data SeqSubBlock
   = Matrix          [String]
   | Format          CharacterFormat
   | Dims            DimensionsFormat
   -- | Items           ItemType
   | Eliminate       String
   | CharStateLabels [CharStateFormat]
   | IgnSSB          String
   | Taxa            [String]
   deriving (Show)

-- | StepMatrix is type to be pulled from Assumptions block. 
data StepMatrix
   = StepMatrix
   { matrixType :: String -- Actually, the name of the tcm. Originally was to be step or real. Collected, but unused.
   , matrixSize :: Int    -- for validation
   , matrixData :: TCM
   } deriving (Show)

-- | TaxaSpecification holds the number of taxa and the list of taxa names. It's collected from a taxa block, and appears as a subtype
-- of NexusBlock. Used for verification.
data TaxaSpecification
   = TaxaSpecification
   { taxaDims   :: Int
   , taxaLabels :: [String]
   } deriving (Show)

type TreeName       = String
type SerializedTree = String

data TreeBlock
   = TreeBlock
   { translate :: [[String]]
   , trees     :: [(TreeName, [NewickForest])]
   } deriving (Show)

data TreeField
   = Translation [String]
   | Tree        (TreeName, [NewickForest])
   | IgnTF       String

data SequenceBlock
   = SequenceBlock
   { meta     :: CharacterMetadata
   , elimList :: String
   , seqs     :: [V.Vector (String, [String])]
   } deriving (Show)