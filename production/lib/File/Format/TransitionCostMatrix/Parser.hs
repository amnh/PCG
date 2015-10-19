{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Parser where

import Data.List.Utility     (duplicates,mostCommon)
import Data.Matrix           (Matrix,fromList)
import Data.Maybe            (catMaybes,fromJust)
import Data.Char             (isSpace)
import Text.Parsec
import Text.Parsec.Custom

data ParseResult 
   = ParseResult [String] [[Double]]

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

matrixBlock :: Stream s m Char => ParsecT s u m [[Double]]
matrixBlock = validateMatrix =<< many (symbol matrixRow)
  where
    matrixRow   = (matrixEntry <* inlineSpaces) `manyTill` eol
    matrixEntry = decimal

validateParseResult :: Stream s m Char => ParseResult -> ParsecT s u m TCM
validateParseResult (ParseResult alphabet protoMatrix)
  | null errors = pure . TCM alphabet . fromList size size $ concat protoMatrix
  | otherwise   = fails errors
  where
    size = succ $ length alphabet 
    rows = length protoMatrix
    badCols = foldr f [] $ zip [(1::Int)..] protoMatrix
    f (n,e) a = let x = length e in if x /= size then (n,x):a else a  
    g (x,y) = (:) (Just $ "Matrix row "++show x++" has "++show y++" columns but "++show size++" columns were expected")
    errors = alphabetErrors ++ matrixErrors
    alphabetErrors = catMaybes   [emptyAlphabet,doubleAlphabet]
    matrixErrors   = catMaybes $ [emptyMatrix,badRowCount] ++ badColCount
    emptyAlphabet  = case alphabet of 
                       [] -> Just "No alphabet specified"
                       _  -> Nothing
    doubleAlphabet = case duplicates alphabet of
                       [] -> Nothing
                       xs -> Just $ "The following symbols were listed multiple times in the custom alphabet: " ++ show xs
    emptyMatrix    = case protoMatrix of
                       [] -> Just "No matrix specified"
                       _  -> Nothing
    badRowCount    = if   rows /= size 
                     then Just $ "Matrix has "++show rows++" rows but "++show size++" rows were expected"
                     else Nothing
    badColCount    = foldr g [] badCols

validateAlphabet :: Stream s m Char => [String] -> ParsecT s u m [String]
validateAlphabet alphabet
  | emptyAlphabet   = fail   "No alphabet specified"
  | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " ++ show dupes
  | otherwise       = pure alphabet 
  where
    emptyAlphabet   = null alphabet
    duplicatesExist = not $ null dupes
    dupes           = duplicates alphabet

validateMatrix :: Stream s m Char => [[Double]] -> ParsecT s u m [[Double]]
validateMatrix matrix
  | null matrix        = fail "No matrix specified"
  | null matrixErrors  = pure matrix
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
