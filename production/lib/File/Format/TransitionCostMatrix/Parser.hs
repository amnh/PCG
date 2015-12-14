{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Parser where

import Data.Char              (isSpace)
import Data.List.Utility      (duplicates,mostCommon)
import Data.Matrix            (Matrix,fromList,ncols,nrows)
import Data.Maybe             (catMaybes,fromJust)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim   (MonadParsec)

data ParseResult 
   = ParseResult [String] (Matrix Double)

data TCM 
   = TCM
   { customAlphabet  :: [String]
   , transitionCosts ::  Matrix Double -- n+1 X n+1 matrix where n = length customAlphabet
   } deriving (Show)

-- | Parses the entirety of a stream producing a TCM result.
-- The result will contain an Alphabet with no duplicate elements
-- and a square Matrix with dimension @(n+1) x (n+1)@ where @n@ is
-- the length of the Alphabet.
tcmStreamParser :: MonadParsec s m Char => m TCM
tcmStreamParser = validateParseResult =<< tcmDefinition <* eof

-- | Parses an intermediary result consisting of an Alphabet and a Matrix.
-- Both the Alphabet and Matrix have been validated independantly for
-- consistencey, but no validation has been performed to ensure that the
-- dimensions of the Matrix and the length of the Alphabet are consistant
-- with each other. 
tcmDefinition :: MonadParsec s m Char => m ParseResult
tcmDefinition = do
    _        <- space
    alphabet <- symbol $ tcmAlphabet
    matrix   <- symbol $ tcmMatrix
    pure $ ParseResult alphabet matrix

-- | Shorthand for the expected format of the alphabet lin in a TCM file.
-- The same as 'alphabetLine inlineSpace'.
tcmAlphabet :: MonadParsec s m Char => m [String]
tcmAlphabet = alphabetLine inlineSpace

-- | Shorthand for the expected format of the matrix block in a TCM file
-- The same as 'matrixBlock inlineSpace'.
tcmMatrix   :: MonadParsec s m Char => m (Matrix Double)
tcmMatrix   = matrixBlock  inlineSpace

-- | The 'alphabetLine' function takes a combinator to consume delimiters between
-- entries in the alphabet line.
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (alphabetLine inlineSpace) "" "a b c d\n"
-- Right ["a","b","c","d"]
--
-- >>> parse (alphabetLine (inlineSpace *> char '|' <* inlineSpace)) "" "2 | 3 | 5 | 7\n"
-- Right ["2","3","5","7"]
alphabetLine :: MonadParsec s m Char => m () -> m [String]
alphabetLine spacing = validateAlphabet =<< (alphabetSymbol <* spacing) `manyTill` endOfLine
  where
    alphabetSymbol = some nonSpace
    nonSpace       = satisfy (not . isSpace)

matrixBlock :: MonadParsec s m Char => m () -> m (Matrix Double)
matrixBlock spacing = validateMatrix =<< many (symbol matrixRow)
  where
    matrixRow   = (matrixEntry <* spacing) `manyTill` endOfLine
    matrixEntry = double

validateParseResult :: MonadParsec s m Char => ParseResult -> m TCM
validateParseResult (ParseResult alphabet matrix)
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
                 , show (size+1)
                 , " x "
                 , show (size+1)
                 , "."
                 ]

validateAlphabet :: MonadParsec s m Char => [String] -> m [String]
validateAlphabet alphabet
  | emptyAlphabet   = fail   "No alphabet specified"
  | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " ++ show dupes
  | otherwise       = pure alphabet 
  where
    emptyAlphabet   = null alphabet
    duplicatesExist = not $ null dupes
    dupes           = duplicates alphabet

validateMatrix :: MonadParsec s m Char => [[Double]] -> m (Matrix Double)
validateMatrix matrix
  | null matrix        = fail "No matrix specified"
  | null matrixErrors  = pure . fromList rows cols $ concat matrix
  | otherwise          = fails matrixErrors
  where
    rows               = length matrix
    cols               = fromJust . mostCommon $ length <$> matrix
    badCols            = foldr getBadCols [] $ zip [(1::Int)..] matrix
    getBadCols (n,e) a = let x = length e in if x /= cols then (n,x):a else a  
    colMsg (x,y)       = (:) (Just $ "Matrix row "++show x++" has "++show y++" columns but "++show cols++" columns were expected")
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

symbol  :: MonadParsec s m Char => m a -> m a
symbol  x = x <* space
