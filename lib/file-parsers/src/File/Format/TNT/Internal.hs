----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Internal types and functions for TNT parseing. Only a subset of types
-- should be exported from top level module.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module File.Format.TNT.Internal
  ( TntResult
  , TreeOnly
  , WithTaxa(..)
  , CCode
  , CCodeAugment(..)
  , CharacterState(..)
  , CharacterSet(..)
  , CNames
  , CharacterName(..)
  , Cost(..)
  , NStates(..)
  , TRead
  , TReadTree
  , LeafyTree(..)
  , NodeType(..)
  , XRead(..)
  , TaxonInfo
  , TaxonName
  , TaxonSequence
  , TntCharacter(..)
  , TntContinuousCharacter
  , TntDiscreteCharacter
  , TntDnaCharacter
  , TntProteinCharacter
  , CharacterMetadata(..)
  , bitsToFlags
  , characterIndicies
  , characterStateChar
  , deserializeStateDiscrete
  , deserializeStateDna
  , deserializeStateProtein
  , discreteStateValues
  , dnaStateValues
  , findFirstSet
  , flexibleNonNegativeInt
  , flexiblePositiveInt
  , initialMetaData
  , keyword
  , metaDataTemplate
  , modifyMetaDataState
  , modifyMetaDataNames
  , modifyMetaDataTCM
  , nonNegInt
  , proteinStateValues
  , serializeStateDiscrete
  , symbol
  , trim
  , whitespace
  , whitespaceInline
  ) where

import           Control.Monad              ((<=<))
import           Data.Bits
import           Data.CaseInsensitive       (FoldCase)
import           Data.Char                  (isAlpha, isLower, isUpper, toLower, toUpper)
import           Data.Foldable
import           Data.Functor               (($>))
import           Data.Key                   (lookup, (!))
import           Data.List                  (inits)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Map                   (Map, assocs, insert, insertWith, keys, union)
import qualified Data.Map                   as M (fromList)
import           Data.Matrix.NotStupid      (Matrix)
import           Data.Proxy
import           Data.Scientific            (floatingOrInteger)
import           Data.Tuple                 (swap)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V (fromList)
import           Data.Word                  (Word32, Word64, Word8)
import           Prelude                    hiding (lookup)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal, scientific, signed)
import           Text.Megaparsec.Custom


-- |
-- The result of parsing a TNT file.
-- A file can contain either only tree data with labeled leaf nodes or
-- a collection of taxa sequences with coresponsing metadata and possibly
-- corresponding tree data.
type  TntResult = Either TreeOnly WithTaxa


-- |
-- The possible parse result when the file contains only tree data.
type  TreeOnly = TRead


-- |
-- The possible parse result when the file contains taxa sequences.
-- 'trees' represents a (possibly empty) forest where each tree in
-- the forest must have the complete taxa set as it's leaf node set.
data  WithTaxa
    = WithTaxa
    { sequences    :: Vector TaxonInfo
    , charMetaData :: Vector CharacterMetadata
    , trees        :: [LeafyTree TaxonInfo]
    }
    deriving stock (Show)


-- CCode types
--------------------------------------------------------------------------------


-- |
-- Parse result from a CCODE command that represents a list of augmentations to
-- the default metatdata state for some characters.
type  CCode = NonEmpty CCodeAugment


-- |
-- The specifcation of which chracters need which metadata mutations.
data  CCodeAugment
    = CCodeAugment
    { charState :: NonEmpty CharacterState
    , charSet   :: NonEmpty CharacterSet
    }
    deriving stock (Show)


-- |
-- The possible character metatadata values that can be mutated.
data CharacterState
    = Additive
    | NonAdditive
    | Active
    | NonActive
    | Sankoff
    | NonSankoff
    | Weight Int
    | Steps  Int
    deriving stock (Eq, Show)


-- |
-- A nonempty contiguous character range.
data  CharacterSet
    = Single    Int
    | Range     Int Int
    | FromStart Int
    | ToEnd     Int
    | Whole
    deriving stock (Eq, Show)


-- CNames types
--------------------------------------------------------------------------------


-- |
-- A list of names for the states of characters in the taxa sequences.
type  CNames = NonEmpty CharacterName


-- |
-- A specifcation for the state names of a given character in the taxon's sequence.
data  CharacterName
    = CharacterName
    { sequenceIndex       :: Int
    , characterId         :: String
    , characterStateNames :: [String]
    }
    deriving stock (Show)


-- Cost types
--------------------------------------------------------------------------------


-- |
-- A custom TCM derivation for a list of characters in the taxa sequences.
data  Cost
    = Cost
    { costIndicies :: CharacterSet
    , costMatrix   :: Matrix Double
    }
    deriving stock (Eq, Show)



-- NStates types
--------------------------------------------------------------------------------


-- |
-- Specifies how to interpret the various character types in the taxa sequences.
data  NStates
    = DnaStates     Bool
    | NumericStates Int
    | ProteinStates
    | ContinuousStates
    deriving stock (Show)


-- TRead types
--------------------------------------------------------------------------------


-- |
-- Parse result of an internal TREAD command.
-- Specifies a nonempty forest of trees, where the leaf node set must be validated
-- againtst the taxa set from a XREAD command.
type  TRead = NonEmpty TReadTree


-- |
-- A tree with data only at the leaf nodes. Leaf nodes contain different criteria
-- for matching the node against a given taxa from the taxa set.
type  TReadTree = LeafyTree NodeType


{-
-- |
-- A tree which has had each leaf node matched against the taxon from the taxa set.
type TNTTree   = LeafyTree TaxonInfo
-}


-- |
-- A rose tree which only contains data at the leaf nodes.
data  LeafyTree a
    = Leaf a
    | Branch [LeafyTree a]
    deriving stock (Eq, Foldable, Functor, Show, Traversable)


-- |
-- Multiple possible criteria for matching leaf nodes to taxa from the taxa set.
data  NodeType
    = Index  Int
    | Name   String
    | Prefix String
    deriving stock (Eq, Show)


-- XRead types
--------------------------------------------------------------------------------


-- |
-- Parse result of an XREAD command.
data  XRead
    = XRead
    { charCountx :: Int
    , taxaCountx :: Int
    , sequencesx :: NonEmpty TaxonInfo
    }
    deriving stock (Show)


-- |
-- The sequence information for a taxon within the TNT file's XREAD command.
-- Contains the 'TaxonName' and the naive 'TaxonSequence'
type  TaxonInfo = (TaxonName, TaxonSequence)


-- |
-- The name of a taxon in a TNT file's XREAD command.
type  TaxonName = String


-- |
-- The naive sequence of a taxon in a TNT files' XREAD command.
type  TaxonSequence = [TntCharacter]


-- |
-- Different character types are deserialized from sequences segments.
-- After all segments are collected they are de-interleaved into a single
-- 'TaxonSequence'.
data  TntCharacter
    = Continuous TntContinuousCharacter
    | Discrete   TntDiscreteCharacter
    | Dna        TntDnaCharacter
    | Protein    TntProteinCharacter
    deriving stock (Eq)


-- |
-- A 'TntContinuousCharacter' is an real valued character. Continuous
-- characters can have negative values. Continuous values are serialized
-- textually as decimal values or integral values, scientific notation supported.
type  TntContinuousCharacter = Maybe Double


-- |
-- A 'TntDiscreteCharacter' is an integral value in the range '[0..62]'. Discrete
-- values are serialized textualy as one of the 64  values:
-- '[0..9] <> [\'A\'..\'B\'] <> [\'a\'..\'z\'] <> "-?"'.
-- Missing \'?\' represents the empty ambiguity group.
-- Each value coresponds to it's respective bit in the 'Word64'. Ambiguity groups
-- are represented by 'Word64' values with multiple set bits.
newtype TntDiscreteCharacter = TntDis Word64
    deriving newtype (Bits, Eq, Ord, FiniteBits)


-- |
-- A 'TntDnaCharacter' represents a nucleotide ('"ACGT"') as an integral value
-- in the range '[0..3]' respectively. If gap characters (\'-\') are treated as a fifth
-- state then values the additional value '4' is added to the integral range.
-- Discrete values are serialized textualy as the DNA IUPAC codes case-insensitively,
-- along with \'-\' & \'?\' characters representing gap or missing data respecitively.
-- Gap represents an ambiguity group of all possible proteins unless gaps are
-- treated as a fifth state. Missing represents the empty ambiguity group.
newtype TntDnaCharacter = TntDna Word8
    deriving newtype (Bits, Eq, FiniteBits, Ord)


-- |
-- A 'TntProteinCharacter' is an integral value in the range '[0..20]'.
-- Discrete values are serialized textualy as the protein IUPAC codes case-insensitively,
-- along with \'-\' & \'?\' characters representing gap or missing data respecitively.
-- Missing represents the empty ambiguity group.
newtype TntProteinCharacter = TntPro Word32
    deriving newtype (Bits, Eq, FiniteBits, Ord)


-- | (✔)
instance Show TntCharacter where

    show (Continuous x) = show x
    show (Discrete   x) = show x
    show (Dna        x) = show x
    show (Protein    x) = show x


-- | (✔)
instance Show TntDiscreteCharacter where
  show x =
    case x `lookup` serializeStateDiscrete of
      Just c  -> [c]
      Nothing -> "[" <> str <> "]"
    where
      str = (serializeStateDiscrete !) <$> bitsToFlags x


-- | (✔)
instance Show TntDnaCharacter where
  show x = [serializeStateDna ! x]


-- | (✔)
instance Show TntProteinCharacter where
  show x =
    case x `lookup` serializeStateProtein of
      Just c  -> [c]
      Nothing -> "[" <> str <> "]"
    where
      str = (serializeStateProtein !) <$> bitsToFlags x


-- CharacterMetadata types
--------------------------------------------------------------------------------


-- |
-- The metadata of a character specifying the attributes of the character
-- specified in the file.
--
-- Default 'CharacterMetadata' values:
--
-- >>> defaultMetaData
-- CharMeta
--   { characterName   = ""
--   , characterStates = mempty
--   , additive        = False
--   , active          = True
--   , sankoff         = False
--   , weight          = 1
--   , steps           = 1
--   , costTCM         = Nothing
--   }
data  CharacterMetadata
    = CharMeta
    { characterName   :: String
    , characterStates :: Vector String
    , additive        :: Bool --- Mutually exclusive sankoff!
    , active          :: Bool
    , sankoff         :: Bool
    , weight          :: Int
    , steps           :: Int
    , costTCM         :: Maybe (Matrix Double)
    }
    deriving stock (Show)


-- |
-- The default values for 'CharacterMetadata' as specified by the TNT "documentation."
initialMetaData :: CharacterMetadata
initialMetaData = CharMeta
                { characterName   = ""
                , characterStates = mempty
                , additive        = False
                , active          = True
                , sankoff         = False
                , weight          = 1
                , steps           = 1
                , costTCM         = Nothing
                }


-- |
-- Convienece method for generating a 'CharacterMetadata' by specifying a single
-- attribute value and defaulting all the other values.
metaDataTemplate :: CharacterState -> CharacterMetadata
metaDataTemplate state = modifyMetaDataState state initialMetaData


-- |
-- Overwrite the value of the 'CharacterMetadata' with the 'CharacterState' value.
modifyMetaDataState :: CharacterState -> CharacterMetadata -> CharacterMetadata
modifyMetaDataState  Additive     old = old { additive = True , sankoff = False }
modifyMetaDataState  NonAdditive  old = old { additive = False }
modifyMetaDataState  Active       old = old { active   = True  }
modifyMetaDataState  NonActive    old = old { active   = False }
modifyMetaDataState  Sankoff      old = old { additive = False, sankoff = True  }
modifyMetaDataState  NonSankoff   old = old { sankoff  = False }
modifyMetaDataState (Weight n)    old = old { weight   = n     }
modifyMetaDataState (Steps  n)    old = old { steps    = n     }


-- |
-- Overwrite the naming variables of the 'CharacterMetadata'.
modifyMetaDataNames :: CharacterName -> CharacterMetadata -> CharacterMetadata
modifyMetaDataNames charName old =
    old
    { characterName   = characterId charName
    , characterStates = V.fromList $ characterStateNames charName
    }


-- |
-- Overwrite the TCM attribute of the 'CharacterMetadata'.
modifyMetaDataTCM :: Matrix Double -> CharacterMetadata -> CharacterMetadata
modifyMetaDataTCM mat old = old { costTCM = Just mat }


-- |
-- Parses an non-negative integer from a variety of representations.
-- Parses both signed integral values and signed floating values
-- if the value is non-negative and an integer.
flexibleNonNegativeInt :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => String -> m Int
flexibleNonNegativeInt labeling = either coerceFloating coerceIntegral . floatingOrInteger
                              =<< signed whitespace scientific <?> ("positive integer for " <> labeling)
  where
    coerceIntegral x
      | x <  (0::Int) = fail $ concat ["The ",labeling," value (",show x,") is a negative number"]
      | otherwise     = pure $ fromEnum x

--    coerceFloating :: (MonadParsec e s m {- , Token s ~ Char -}) => Double -> m Int
    coerceFloating = assertNonNegative <=< assertIntegral labeling
      where
        assertNonNegative x
          | x >= 0    = pure x
          | otherwise = fail $ concat ["The ",labeling," value (",show x,") is a negative value"]


-- |
-- Parses an positive integer from a variety of representations.
-- Parses both signed integral values and signed floating values
-- if the value is positive and an integer.
--
-- @flexiblePositiveInt labeling@ uses the @labeling@ parameter to
-- improve ParseError generation.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "1\n"
-- Right 1
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0\n"
-- Left 1:2:
-- expecting rest of number
-- The errorCount value (0) is not a positive number
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "42.0\n"
-- Right 42
--
-- >>> parse (flexiblePositiveInt "errorCount") "" "0.1337\n"
-- Left 1:7:
-- expecting 'E', 'e', or rest of number
-- The errorCount value (0.1337) is a real value, not an integral value
-- The errorCount value (0.1337) is not a positive integer
flexiblePositiveInt :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => String -> m Int
flexiblePositiveInt labeling = either coerceFloating coerceIntegral . floatingOrInteger
                             =<< signed whitespace scientific <?> ("positive integer for " <> labeling)
  where
    coerceIntegral x
      | x <= (0::Int) = fail $ concat ["The ",labeling," value (",show x,") is not a positive number"]
      | otherwise     = pure $ fromEnum x

    coerceFloating = assertPositive <=< assertIntegral labeling
      where
        assertPositive x
          | x > 0     = pure x
          | otherwise = fail $ concat ["The ",labeling," value (",show x,") is not a positive integer"]


-- |
-- Consumes a TNT keyword flexibly.
-- @keyword fullName minChars@ will parse the __longest prefix of__ @fullName@
-- requiring that __at least__ the first @minChars@ of @fullName@ are in the prefix.
-- Keyword prefixes are terminated with an `inlinedSpace` which is not consumed by the combinator.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (keyword "abrakadabra" 4) "" "abrakadabra"
-- Right "abrakadabra"
--
-- >>> parse (keyword "abrakadabra" 4) "" "abrakad"
-- Right "abrakadabra"
--
-- >>> parse (keyword "abrakadabra" 4) "" "abra"
-- Right "abrakadabra"
--
-- >>> parse (keyword "abrakadabra" 4) "" "abr"
-- Left 1:1:
-- unexpected "abr"
-- expecting keyword 'abrakadabra'
keyword :: forall e s m. (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => String -> Int -> m ()
keyword x y = abreviatable x y $> ()
  where
    abreviatable fullName minimumChars
      | minimumChars < 1             = fail $ "Nonpositive abbreviation prefix (" <> show minimumChars <> ") supplied to abreviatable combinator"
      | any (not . isAlpha) fullName = fail $ "A keywork containing a non alphabetic character: '" <> show fullName <> "' supplied to abreviateable combinator"
      | otherwise                    = combinator <?> "keyword '" <> fullName <> "'"
      where
        combinator     = choice partialOptions $> fullName
        partialOptions = makePartial <$> drop minimumChars (inits fullName)
        makePartial    = try . (<* terminator) . string' . tokensToChunk (Proxy :: Proxy s)
        terminator     = lookAhead $ satisfy (not . isAlpha)


-- |
-- Parses an Int which is non-negative. This Int is not parsed flexibly.
-- Will fail integral valued Double literals. Use this in preference to 'flexibleNonNegativeInt'
-- when expecting one of these chars ".eE" adjacent to the Int value.
nonNegInt :: (MonadParsec e s m, Token s ~ Char) => m Int
nonNegInt = decimal


-- |
-- Parses an Integral value from a 'Double' value. If the 'Double' is not an
-- integral value, then a parse error is raised. The first 'String' parameter
-- is used as a label in the error reporting.
assertIntegral :: MonadFail m => String -> Double -> m Int
assertIntegral labeling x
  | isInt x   = pure $ fromEnum rounded
  | otherwise = fail $ fold
      [ "The ", labeling, " value (", show x, ") is a real value, not an integral value" ]
  where
    isInt n = n == fromInteger rounded
    rounded = round x


-- |
-- Parses a single character index or a contiguous character range.
characterIndicies :: (MonadParsec e s m, Token s ~ Char) => m CharacterSet
characterIndicies = choice $ try <$> [range, fromStart, singleVal, toEnd, whole]
  where
    range     = Range     <$> num <* dot <*> num
    fromStart = FromStart <$> num <* dot
    singleVal = Single    <$> num
    toEnd     = dot *> (ToEnd <$> num)
    whole     = dot $>  Whole
    num       = symbol (nonNegInt <?> "sequence index value")
    dot       = symbol (char '.')


-- |
-- Parses one of the valid character states for a TNT file.
characterStateChar :: (MonadParsec e s m, Token s ~ Char) => m Char
characterStateChar = oneOf (toList discreteStateValues)


-- |
-- The only 64 (case-insensitive) valid state values for a TNT file.
discreteStateValues :: Vector Char
discreteStateValues = V.fromList $ fold [['0'..'9'], ['A'..'Z'], ['a'..'z'], "-", "?"]


-- |
-- The only valid state values for a DNA character.
dnaStateValues :: Vector Char
dnaStateValues = V.fromList $ keys deserializeStateDna


-- |
-- The only valid state values for a protein character.
proteinStateValues :: Vector Char
proteinStateValues = V.fromList $ keys deserializeStateProtein


-- |
-- A map for serializing discrete state chatracters.
serializeStateDiscrete :: Map TntDiscreteCharacter Char
serializeStateDiscrete = swapMap deserializeStateDiscrete


-- |
-- A map for deserializing discrete state chatracters.
deserializeStateDiscrete :: Map Char TntDiscreteCharacter
deserializeStateDiscrete = insert '?' allBits core
  where
    allBits = foldl (.|.) zeroBits core
    core    = M.fromList $ zip (toList discreteStateValues) (bit <$> [0..])


-- |
-- A map for serializing dna state chatracters.
serializeStateDna :: Map TntDnaCharacter Char
serializeStateDna = swapMap deserializeStateDna


-- |
-- A map for deserializing dna state chatracters.
deserializeStateDna :: Map Char TntDnaCharacter
deserializeStateDna = casei core
  where
    ref  = (core !)
    core = M.fromList
         [ ('A', bit 0   )
         , ('G', bit 1   )
         , ('C', bit 2   )
         , ('T', bit 3   )
         , ('-', bit 4   ) -- assume 5th state
         , ('U', ref 'T' )
         , ('R', ref 'A' .|. ref 'G')
         , ('M', ref 'A' .|. ref 'C')
         , ('W', ref 'A' .|. ref 'T')
         , ('S', ref 'G' .|. ref 'C')
         , ('K', ref 'G' .|. ref 'T')
         , ('T', ref 'C' .|. ref 'T')
         , ('V', ref 'A' .|. ref 'G' .|. ref 'C')
         , ('D', ref 'A' .|. ref 'G' .|. ref 'T')
         , ('H', ref 'A' .|. ref 'C' .|. ref 'T')
         , ('B', ref 'G' .|. ref 'C' .|. ref 'T')
         , ('N', ref 'A' .|. ref 'G' .|. ref 'C' .|. ref 'T')
         , ('X', ref 'N')
         , ('?', ref 'A' .|. ref 'G' .|. ref 'C' .|. ref 'T' .|. ref '-')
         ]


-- |
-- A map for deserializing protein state chatracters.
serializeStateProtein :: Map TntProteinCharacter Char
serializeStateProtein = swapMap deserializeStateProtein


-- |
-- A map for deserializing protein state chatracters.
deserializeStateProtein :: Map Char TntProteinCharacter
deserializeStateProtein = insert '?' allBits . casei $ core `union` multi
  where
    core    = M.fromList $ zip "ACDEFGHIKLMNPQRSTVWY-" (bit <$> [0..])
    multi   = M.fromList [('B', ref 'D' .|. ref 'N'), ('Z', ref 'E' .|. ref 'Q'), ('X', allBits `clearBit` gapBit )]
    ref     = (core !)
    allBits = foldl (.|.) zeroBits core
    gapBit  = findFirstSet $ core ! '-'


-- |
-- Add case insensitive keys to map with the corresponding keys.
casei :: Map Char v -> Map Char v
casei x = foldl f x $ assocs x
  where
    f m (k,v)
      | isLower k = insertWith g (toUpper k) v m
      | isUpper k = insertWith g (toLower k) v m
      | otherwise = m
    g _ o = o -- Don't overwrite old, opisite case values in the map.


-- |
-- Gets the set bit flags of all bits in the finite bit structure.
-- A useful decunstruction to supply to folds.
bitsToFlags :: FiniteBits b => b -> [b]
bitsToFlags b
  | zeroBits == b = []
  | otherwise     = bit i : bitsToFlags (b `clearBit` i)
  where
    i = findFirstSet b


-- |
-- Gets the index of the least significant set bit.
findFirstSet :: FiniteBits b => b -> Int
findFirstSet = countTrailingZeros


-- |
-- Inverts a map.
swapMap :: Ord a => Map k a -> Map a k
swapMap x = let !tups = assocs x
            in M.fromList $ swap <$> tups

-- |
-- Consumes trailing whitespace after the parameter combinator.
symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol c = c <* whitespace


-- |
-- Consumes trailing whitespace after the parameter combinator.
trim :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trim c = whitespace *> c <* whitespace


-- |
-- Consumes zero or more whitespace characters __including__ line breaks.
{-# INLINE whitespace #-}
whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = space


-- |
-- Consumes zero or more whitespace characters that are not line breaks.
{-# INLINE whitespaceInline #-}
whitespaceInline :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespaceInline =  inlinedSpace
