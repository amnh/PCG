{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Parser where

import Data.List.Utility     (duplicates,mostCommon)
import Data.Matrix           (Matrix,fromList,ncols,nrows)
import Data.Maybe            (catMaybes,fromJust)
import Data.Char             (isSpace)
import Text.Parsec
import Text.Parsec.Custom

data ParseResult 
   = ParseResult [String] (Matrix Double)

data TCM 
   = TCM
   { customAlphabet  :: [String]
   , transitionCosts ::  Matrix Double -- n+1 X n+1 matrix where n = length customAlphabet
   } deriving (Show)

tcmStreamParser :: Stream s m Char => ParsecT s u m TCM
tcmStreamParser = validateParseResult =<< tcmDefinition <* eof

tcmDefinition :: Stream s m Char => ParsecT s u m ParseResult
tcmDefinition = do
    _        <- spaces
    alphabet <- symbol alphabetLine
    matrix   <- symbol matrixBlock
    pure $ ParseResult alphabet matrix

alphabetLine :: Stream s m Char => ParsecT s u m [String]
alphabetLine = validateAlphabet =<< (alphabetSymbol <* inlineSpaces) `manyTill` eol
  where
    alphabetSymbol = many1 nonSpace
    nonSpace       = satisfy (not . isSpace)

matrixBlock :: Stream s m Char => ParsecT s u m (Matrix Double)
matrixBlock = validateMatrix =<< many (symbol matrixRow)
  where
    matrixRow   = (matrixEntry <* inlineSpaces) `manyTill` eol
    matrixEntry = decimal

validateParseResult :: Stream s m Char => ParseResult -> ParsecT s u m TCM
validateParseResult (ParseResult alphabet matrix)
  | dimMismatch  = pure $ TCM alphabet matrix
  | otherwise    = fail errorMessage
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

validateAlphabet :: Stream s m Char => [String] -> ParsecT s u m [String]
validateAlphabet alphabet
  | emptyAlphabet   = fail   "No alphabet specified"
  | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " ++ show dupes
  | otherwise       = pure alphabet 
  where
    emptyAlphabet   = null alphabet
    duplicatesExist = not $ null dupes
    dupes           = duplicates alphabet

validateMatrix :: Stream s m Char => [[Double]] -> ParsecT s u m (Matrix Double)
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
    matrixErrors       = catMaybes $ [badRowCount] ++ badColCount
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

symbol  :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
symbol  x = x <* spaces
