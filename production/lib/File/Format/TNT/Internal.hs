{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts #-}
module File.Format.TNT.Internal where

import           Control.Monad            ((<=<))
import           Data.Char                (isAlpha,isSpace)
import           Data.Foldable            (toList)
import           Data.List                (inits)
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Matrix.NotStupid    (Matrix)
import           Data.Maybe               (catMaybes)
import           Data.Vector              (Vector,fromList)
import           Data.Word                (Int64)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

--Export types
{-
data Hennig
   = Hennig
   { taxaCount    :: Int
   , sequences    :: NonEmpty TaxonInfo
   , charMetaData :: Vector CharacterMetaData
   } deriving (Show)
-}

type TntResult = Either TreeOnly WithTaxa
type TreeOnly  = TRead
type Yucky     = String
data WithTaxa
   = WithTaxa
   { sequences    :: Vector TaxonInfo
   , charMetaData :: Vector CharacterMetaData
   , trees        :: [LeafyTree TaxonInfo]
   } deriving (Show)

-- CCode types
--------------------------------------------------------------------------------

data CCode
   = CCode
   { charState :: CharacterState
   , charSet   :: NonEmpty CharacterSet
   } deriving (Show)

data CharacterState
   = Additive
   | NonAdditive
   | Active
   | NonActive
   | Sankoff
   | NonSankoff
   | Weight Int
   | Steps  Int
   deriving (Show)

data CharacterSet
   = Single    Int
   | Range     Int Int
   | FromStart Int
   | ToEnd     Int
   | Whole
   deriving (Eq,Show)

-- CNames types
--------------------------------------------------------------------------------

type CNames = NonEmpty CharacterName

data CharacterName
   = CharacterName
   { sequenceIndex       :: Int
   , characterId         :: String
   , characterStateNames :: [String]
   } deriving (Show)

-- Cost types
--------------------------------------------------------------------------------

data Cost
   = Cost
   { costIndicies :: CharacterSet
   , costMatrix   :: Matrix Double
   } deriving (Eq,Show)

-- TRead types
--------------------------------------------------------------------------------

type TRead     = NonEmpty  TReadTree
type TReadTree = LeafyTree NodeType
type TNTTree   = LeafyTree TaxonInfo

data LeafyTree a
   = Leaf a
   | Branch [LeafyTree a]
   deriving (Eq,Foldable,Functor,Show,Traversable)

data NodeType
   = Index  Int
   | Name   String
   | Prefix String
   deriving (Eq,Show)

--XRead types
--------------------------------------------------------------------------------

data XRead
   = XRead
   { charCountx :: Int
   , taxaCountx :: Int
   , sequencesx :: NonEmpty TaxonInfo
   } deriving (Show)

-- | The sequence information for a taxon within the TNT file's XREAD command.
-- Contains the 'TaxonName' and the naive 'TaxonSequence' 
type TaxonInfo     = (TaxonName, TaxonSequence)

-- | The name of a taxon in a TNT file's XREAD command.
type TaxonName     = String

-- | The naive sequence of a taxon in a TNT files' XREAD command.
type TaxonSequence = [TntCharacter]

-- | Different character types are deserialized from sequences segments.
--   After all segments are collected they are de-interleaved into a single
--   'TaxonSequence'.
type TaxonSequenceSegment = (TaxonName, Either [TntContinuousCharacter] [TntDiscreteCharacter])

-- | A TNT character is either a continuous value represented by a 'Double'
type TntCharacter = Either TntContinuousCharacter TntDiscreteCharacter

-- | A TntDiscreteCharacter is an integral value in the range [0..63]. Discrete
--   values are serialized textualy as one of the 64  values:
--   '[0..9] ++ [\'A\'..\'B\'] ++ [\'a\'..\'z\'] ++ "-?"'.
--   Each value coresponds to it's respective bit in the 'Int64'. Ambiguity groups
--   are represented byt 'Int64' values with multiple set bits.
type TntDiscreteCharacter = Int64

type TntContinuousCharacter = Double

-- CharacterMetaData types
--------------------------------------------------------------------------------

data CharacterMetaData
   = CharMeta
   { characterName   :: String
   , characterStates :: Vector String
   , additive        :: Bool --- Mutually exclusive sankoff
   , active          :: Bool
   , sankoff         :: Bool
   , weight          :: Int
   , steps           :: Int
   , costTCM         :: Maybe (Matrix Double)
   } deriving (Show)

initialMetaData :: CharacterMetaData
initialMetaData = CharMeta "" mempty False True False 1 1 Nothing

metaDataTemplate :: CharacterState -> CharacterMetaData
metaDataTemplate state = modifyMetaDataState state initialMetaData

modifyMetaDataState :: CharacterState -> CharacterMetaData -> CharacterMetaData
modifyMetaDataState  Additive     old = old { additive = True , sankoff = False }
modifyMetaDataState  NonAdditive  old = old { additive = False }
modifyMetaDataState  Active       old = old { active   = True  }
modifyMetaDataState  NonActive    old = old { active   = False }
modifyMetaDataState  Sankoff      old = old { additive = False, sankoff = True  }
modifyMetaDataState  NonSankoff   old = old { sankoff  = False }
modifyMetaDataState (Weight n)    old = old { weight   = n     }
modifyMetaDataState (Steps  n)    old = old { steps    = n     }

modifyMetaDataNames :: CharacterName -> CharacterMetaData -> CharacterMetaData
modifyMetaDataNames charName old = old { characterName = characterId charName, characterStates = fromList $ characterStateNames charName }

modifyMetaDataTCM :: Matrix Double -> CharacterMetaData -> CharacterMetaData
modifyMetaDataTCM mat old = old { costTCM = Just mat }

-- | Parses an non-negative integer from a variety of representations.
-- Parses both signed integral values and signed floating values
-- if the value is non-negative and an integer.
--
flexibleNonNegativeInt :: MonadParsec s m Char => String -> m Int
flexibleNonNegativeInt labeling = either coerceIntegral coerceFloating
                              =<< signed whitespace number <?> ("positive integer for " ++ labeling)
  where
    coerceIntegral :: MonadParsec s m Char => Integer -> m Int
    coerceIntegral x
      | x <  0      = fail $ concat ["The ",labeling," value (",show x,") is a negative number"]
      | otherwise   = pure $ fromEnum x
    coerceFloating :: MonadParsec s m Char => Double -> m Int
    coerceFloating = assertNonNegative <=< assertIntegral labeling
      where
        assertNonNegative x
          | x >= 0    = pure x
          | otherwise = fail $ concat ["The ",labeling," value (",show x,") is a negative value"]

-- | Parses an positive integer from a variety of representations.
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
flexiblePositiveInt :: MonadParsec s m Char => String -> m Int
flexiblePositiveInt labeling = either coerceIntegral coerceFloating
                             =<< signed whitespace number <?> ("positive integer for " ++ labeling)
  where
    coerceIntegral :: MonadParsec s m Char => Integer -> m Int
    coerceIntegral x
      | x <= 0      = fail $ concat ["The ",labeling," value (",show x,") is not a positive number"]
      | otherwise   = pure $ fromEnum x
    coerceFloating :: MonadParsec s m Char => Double -> m Int
    coerceFloating = assertPositive <=< assertIntegral labeling
      where
        assertPositive x
          | x > 0     = pure x
          | otherwise = fail $ concat ["The ",labeling," value (",show x,") is not a positive integer"]

-- | Consumes a TNT keyword flexibly.
--   @keyword fullName minChars@ will parse the __longest prefix of__ @fullName@
--   requiring that __at least__ the first @minChars@ of @fullName@ are in the prefix.
--   Keyword prefixes are terminated with an `inlineSpace` which is not consumed by the combinator.
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
keyword :: MonadParsec s m Char => String -> Int -> m ()
keyword x y = abreviatable x y *> pure ()
  where
    abreviatable :: MonadParsec s m Char => String -> Int -> m String
    abreviatable fullName minimumChars
      | minimumChars < 1             = fail $ "Nonpositive abreviation prefix (" ++ show minimumChars ++ ") supplied to abreviatable combinator"
      | any (not . isAlpha) fullName = fail $ "A keywork containing a non alphabetic character: '" ++ show fullName ++ "' supplied to abreviateable combinator"
      | otherwise                    = combinator <?> "keyword '" ++ fullName ++ "'"
      where
        combinator      = choice partialOptions *> pure fullName
        partialOptions  = makePartial <$> drop minimumChars (inits fullName)
        makePartial opt = try $ string' opt <* terminator
        terminator      = lookAhead $ satisfy (not . isAlpha) 

-- | Parses an Int which is non-negative. This Int is not parsed flexibly.
--   Will fail integral valued Double literals. Use this in preference to 'flexibleNonNegativeInt'
--   when expecting one of these chars ".eE" adjacent to the Int value.
nonNegInt :: MonadParsec s m Char => m Int
nonNegInt = fromIntegral <$> integer

assertIntegral :: MonadParsec s m Char => String -> Double -> m Int
assertIntegral labeling x
  | isInt x   = pure $ fromEnum rounded
  | otherwise = fail $ concat ["The ",labeling," value (",show x,") is a real value, not an integral value"]
  where
    isInt n = n == fromInteger rounded
    rounded = round x

-- | Parses a single character index or a contiguous character range
characterIndicies :: MonadParsec s m Char => m CharacterSet
characterIndicies = choice $ try <$> [range, fromStart, single, toEnd, whole]
  where
    range     = Range     <$> num <* dot <*> num
    fromStart = FromStart <$> num <* dot
    single    = Single    <$> num
    toEnd     = dot *> (ToEnd <$> num)
    whole     = dot *> pure Whole
    num       = symbol (nonNegInt <?> "sequence index value")
    dot       = symbol (char '.')

-- | Parses one of the valid character states for a TNT file
characterStateChar :: MonadParsec s m Char => m Char
characterStateChar = oneOf (toList characterStateValues)

-- | The only 64 valid state characters for a TNT file
characterStateValues :: Vector Char
characterStateValues = fromList $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-?"

-- | Consumes trailing whitespace after the parameter combinator.
symbol :: MonadParsec s m Char => m a -> m a
symbol c = c <* whitespace

-- | Consumes trailing whitespace after the parameter combinator.
trim :: MonadParsec s m Char => m a -> m a
trim c = whitespace *> c <* whitespace

-- | Consumes zero or more whitespace characters __including__ line breaks.
whitespace :: MonadParsec s m Char => m ()
whitespace = space

-- | Consumes zero or more whitespace characters that are not line breaks.
whitespaceInline :: MonadParsec s m Char => m ()
whitespaceInline =  inlineSpace
