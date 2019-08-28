-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TransitionCostMatrix.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing TCM files into an alphabet and square matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module File.Format.TransitionCostMatrix.Parser
  ( TCM(..)
  , alphabetLine
  , tcmAlphabet
  , tcmMatrix
  , tcmStreamParser
  , matrixBlock
  ) where

import           Control.Applicative.Combinators.NonEmpty
import           Control.DeepSeq
import           Data.Char                                (isSpace)
import           Data.Data
import           Data.Foldable
import           Data.List.NonEmpty                       (NonEmpty)
import           Data.List.Utility                        (duplicates, mostCommon)
import           Data.Matrix.NotStupid                    (Matrix, ncols, nrows)
import qualified Data.Matrix.NotStupid                    as M (fromList)
import           Data.Maybe                               (catMaybes, fromJust)
import           Data.String
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as LT
import           Data.Text.Short                          (ShortText)
import           Data.Vector.NonEmpty                     (Vector, fromNonEmpty)
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec                          hiding (someTill)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer               (scientific)
import           Text.Megaparsec.Custom


-- |
-- Intermediate parse result prior to consistancy validation
data TCMParseResult
   = TCMParseResult (Vector ShortText) (Matrix Rational) deriving (Show)


-- |
-- The results of a TCM file consisting of
--
--   * A custom alphabet of "Symbols"
--
--   * A matrix consisting of the transition costs between symbols
--
-- The following equality will hold for an 'TCM':
--
-- > (length . customAlphabet) tcm == (nrows . transitionCosts) tcm && (length . customAlphabet) tcm == (ncols . transitionCosts) tcm
--
-- Note that the 'transitionCosts` does not need to be a symetic matrix nor have identity values on the matrix diagonal.
data  TCM
    = TCM
    { customAlphabet  :: Vector ShortText -- ^ The custom alphabet of "Symbols" for which the TCM matrix is defined
    , transitionCosts :: Matrix Rational  -- ^ The cost to transition between any two symbols, square but not necessarily symetric
    } -- n+1 X n+1 matrix where n = length customAlphabet
    deriving stock    (Eq, Generic, Show, Typeable)
    deriving anyclass (NFData)


-- |
-- Parses the entirety of a stream producing a TCM result.
-- The result will contain an Alphabet with no duplicate elements
-- and a square Matrix with dimension @(n+1) x (n+1)@ where @n@ is
-- the length of the Alphabet.
{-# INLINEABLE tcmStreamParser #-}
{-# SPECIALISE tcmStreamParser :: Parsec Void  T.Text TCM #-}
{-# SPECIALISE tcmStreamParser :: Parsec Void LT.Text TCM #-}
{-# SPECIALISE tcmStreamParser :: Parsec Void  String TCM #-}
tcmStreamParser :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m TCM
tcmStreamParser = validateTCMParseResult =<< tcmDefinition <* eof


-- |
-- Parses an intermediary result consisting of an Alphabet and a Matrix.
-- Both the Alphabet and Matrix have been validated independantly for
-- consistencey, but no validation has been performed to ensure that the
-- dimensions of the Matrix and the length of the Alphabet are consistant
-- with each other.
{-# INLINE tcmDefinition #-}
{-# SPECIALISE tcmDefinition :: Parsec Void  T.Text TCMParseResult #-}
{-# SPECIALISE tcmDefinition :: Parsec Void LT.Text TCMParseResult #-}
{-# SPECIALISE tcmDefinition :: Parsec Void  String TCMParseResult #-}
tcmDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m TCMParseResult
tcmDefinition = do
    _        <- space
    alphabet <- symbol tcmAlphabet
    matrix   <- symbol tcmMatrix
    pure $ TCMParseResult alphabet matrix


-- |
-- Shorthand for the expected format of the alphabet lin in a TCM file.
-- The same as 'alphabetLine inlinedSpace'.
{-# INLINEABLE tcmAlphabet #-}
{-# SPECIALISE tcmAlphabet :: Parsec Void  T.Text (Vector ShortText) #-}
{-# SPECIALISE tcmAlphabet :: Parsec Void LT.Text (Vector ShortText) #-}
{-# SPECIALISE tcmAlphabet :: Parsec Void  String (Vector ShortText) #-}
tcmAlphabet :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Vector ShortText)
tcmAlphabet = alphabetLine inlinedSpace


-- |
-- Shorthand for the expected format of the matrix block in a TCM file
-- The same as 'matrixBlock inlinedSpace'.
{-# INLINEABLE tcmMatrix #-}
{-# SPECIALISE tcmMatrix :: Parsec Void  T.Text (Matrix Rational) #-}
{-# SPECIALISE tcmMatrix :: Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALISE tcmMatrix :: Parsec Void  String (Matrix Rational) #-}
tcmMatrix :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Matrix Rational)
tcmMatrix = matrixBlock inlinedSpace


-- |
-- The 'alphabetLine' function takes a combinator to consume delimiters between
-- elements in the alphabet line and returns a list of elements in the alphabet.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (alphabetLine inlinedSpace) "" "a b c d\n"
-- Right ["a","b","c","d"]
--
-- >>> parse (alphabetLine (inlinedSpace *> char '|' <* inlinedSpace)) "" "2 | 3 | 5 | 7\n"
-- Right ["2","3","5","7"]
{-# INLINEABLE alphabetLine #-}
{-# SPECIALISE alphabetLine :: Parsec Void  T.Text () -> Parsec Void  T.Text (Vector ShortText) #-}
{-# SPECIALISE alphabetLine :: Parsec Void LT.Text () -> Parsec Void LT.Text (Vector ShortText) #-}
{-# SPECIALISE alphabetLine :: Parsec Void  String () -> Parsec Void  String (Vector ShortText) #-}
alphabetLine :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m () -> m (Vector ShortText)
alphabetLine spacing = validateAlphabet =<< ((alphabetSymbol <* spacing) `someTill` endOfLine)
  where
    alphabetSymbol = takeWhile1P Nothing (not . isSpace)


-- |
-- The 'matrixBlock' function takes a combinator to consume delimiters between
-- entries in a line of the matrix and returns a square 'Matrix Rational'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (matrixBlock inlinedSpace) "" "1 2 3 \n 4 5 6\n7 8 9\n"
-- Right (( 1 2 3 )
--        ( 4 5 6 )
--        ( 7 8 9 ))
--
-- >>> parse (matrixBlock (char ':') "" "1.0:1.0\n1.0:0.0"
-- Right (( 1 2 )
--        ( 3 4 ))
{-# INLINEABLE matrixBlock #-}
{-# SPECIALISE matrixBlock :: Parsec Void  T.Text () -> Parsec Void  T.Text (Matrix Rational) #-}
{-# SPECIALISE matrixBlock :: Parsec Void LT.Text () -> Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALISE matrixBlock :: Parsec Void  String () -> Parsec Void  String (Matrix Rational) #-}
matrixBlock :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m () -> m (Matrix Rational)
matrixBlock spacing = validateMatrix =<< many (symbol matrixRow)
  where
    matrixRow   = (spacing *> matrixEntry <* spacing) `manyTill` endOfLine
    matrixEntry = toRational <$> scientific


-- |
-- Validates that the dimensions of the Matrix are @(n+1) x (n+1)@
-- where @n@ is the length of the Alphabet.
{-# INLINE validateTCMParseResult #-}
{-# SPECIALISE validateTCMParseResult :: TCMParseResult -> Parsec Void  T.Text TCM #-}
{-# SPECIALISE validateTCMParseResult :: TCMParseResult -> Parsec Void LT.Text TCM #-}
{-# SPECIALISE validateTCMParseResult :: TCMParseResult -> Parsec Void  String TCM #-}
validateTCMParseResult :: MonadFail m => TCMParseResult -> m TCM
validateTCMParseResult (TCMParseResult alphabet matrix)
  | dimMismatch  = fail errorMessage
  | otherwise    = pure $ TCM alphabet matrix
  where
    size         = length alphabet
    rows         = nrows matrix
    cols         = ncols matrix
    dimMismatch  = size + 1 /= rows
                || size + 1 /= cols
    errorMessage = concat
        [ "The alphabet length is "
        , show size
        , " but the matrix dimensions are "
        , show rows
        , " x "
        , show cols
        , ". The expected matrix dimensions were "
        , show $ size + 1
        , " x "
        , show $ size + 1
        , "."
        ]


-- |
-- Validates the information contained in the Alphabet.
--
-- Ensures that the Alphabet:
--
--   * Contains no duplicate elements
--
{-# INLINE validateAlphabet #-}
{-# SPECIALISE validateAlphabet :: NonEmpty  T.Text -> Parsec Void  T.Text (Vector ShortText) #-}
{-# SPECIALISE validateAlphabet :: NonEmpty LT.Text -> Parsec Void LT.Text (Vector ShortText) #-}
{-# SPECIALISE validateAlphabet :: NonEmpty  String -> Parsec Void  String (Vector ShortText) #-}
validateAlphabet :: forall e s m . (MonadFail m, MonadParsec e s m, Token s ~ Char) => NonEmpty (Tokens s) -> m (Vector ShortText)
validateAlphabet alphabet
  | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " <> shownDuplicates
  | otherwise       = pure . fromNonEmpty $ toShortText <$> alphabet
  where
    duplicatesExist = not $ null dupes
    dupes           = duplicates $ toList alphabet
    shownDuplicates = show $ chunkToTokens pxy <$> dupes
    toShortText     = fromString . chunkToTokens pxy
    pxy = Proxy :: Proxy s


-- |
-- Validates the information contained in the Matrix constitutes a square matrix.
--
-- Ensures that the Matrix:
--
--   * Is not empty
--
--   * Each row has the same number of columns
--
--   * The number of rows match the number of columns
--
{-# INLINEABLE validateMatrix #-}
{-# SPECIALISE validateMatrix :: [[Rational]] -> Parsec Void  T.Text (Matrix Rational) #-}
{-# SPECIALISE validateMatrix :: [[Rational]] -> Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALISE validateMatrix :: [[Rational]] -> Parsec Void  String (Matrix Rational) #-}
validateMatrix :: (MonadFail m, MonadParsec e s m) => [[Rational]] -> m (Matrix Rational)
validateMatrix matrix
  | null matrix        = fail "No matrix specified"
  | null matrixErrors  = pure . M.fromList rows cols $ concat matrix
  | otherwise          = fails matrixErrors
  where
    rows               = length matrix
    cols               = fromJust . mostCommon $ length <$> matrix
    badCols            = foldr getBadCols [] $ zip [(1::Int)..] matrix
    getBadCols (n,e) a = let x = length e in if x /= cols then (n,x):a else a
    colMsg (x,y)       = (:) (Just $ fold [ "Matrix row ", show x, " has ", show y, " columns but ", show cols, " columns were expected"])
    matrixErrors       = catMaybes $ badRowCount : badColCount
    badColCount        = foldr colMsg [] badCols
    badRowCount        = if   rows == cols
                         then Nothing
                         else Just $ concat
                             [ "The matrix is not a square matrix. The matrix has "
                             , show rows
                             , " rows but "
                             , show cols
                             , " rows were expected"
                             ]


-- |
-- Whitespace consuming combinator wrapper
{-# INLINE symbol #-}
{-# SPECIALISE symbol :: Parsec Void  T.Text a -> Parsec Void  T.Text a #-}
{-# SPECIALISE symbol :: Parsec Void LT.Text a -> Parsec Void LT.Text a #-}
{-# SPECIALISE symbol :: Parsec Void  String a -> Parsec Void  String a #-}
symbol  :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol  x = x <* space
