{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Internal where

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntSet              (IntSet, singleton)
import qualified Data.IntSet        as IS (fromList)
import           Data.List                (intersperse,inits)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | The sequence information for a taxon within the TNT file's XREAD command.
-- Contains the 'TaxonName' and the naive 'TaxonSequence' 
type TaxonInfo     = (TaxonName, TaxonSequence)

-- | The name of a taxon in a TNT file's XREAD command.
type TaxonName     = String

-- | The naive sequence of a taxon in a TNT files' XREAD command.
type TaxonSequence = [[Char]]

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
   = Single Int
   | Range  Int Int
   deriving (Show)

data CharacterChange = Change CharacterState (NonEmpty CharacterSet) deriving (Show)

data CharacterMetaData
   = CharMeta
   { aligned :: Bool
   , active  :: Bool
   , sankoff :: Bool
   , weight  :: Int
   , steps   :: Int
   } deriving (Show)

-- | Parses an Int which is non-negative.
nonNegInt :: MonadParsec s m Char => m Int
nonNegInt = fromIntegral <$> integer

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
    coerceFloating x
      | null errors = pure $ fromEnum rounded
      | otherwise   = fails errors
      where
        errors      = catMaybes $ [posError,intError]
        posError    = if x >= 1  then Nothing else Just $ concat ["The ",labeling," value (",show x,") is not a positive integer"]
        intError    = if isInt x then Nothing else Just $ concat ["The ",labeling," value (",show x,") is a real value, not an integral value"]
        isInt n     = n == fromInteger rounded
        rounded     = round x

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

keyword x y = abreviatable x y *> pure ()

abreviatable :: MonadParsec s m Char => String -> Int -> m String
abreviatable fullName minimumChars =
  if minimumChars < 1
  then fail "Nonpositive abreviation prefix supplied to abreviatable combinator"
  else combinator 
  where
    thenInlineSpace = notFollowedBy notInlineSpace
    notInlineSpace  = satisfy $ \x -> not (isSpace x) || x == '\n' || x ==  '\r'
    (req,opt)  = splitAt minimumChars fullName
    tailOpts   = fmap (\suffix -> try (string' (req ++ suffix) <* thenInlineSpace)) $ inits opt
    combinator = choice tailOpts *> pure fullName
