{-# LANGUAGE FlexibleContexts #-}

module File.Format.TransitionCostMatrix.Parser
  ( TCM()
  , customAlphabet
  , parseTcmStream
  , transitionCosts
  ) where

import Data.List             (sort)
import Data.Matrix           (Matrix,fromList)
import Data.Maybe            (catMaybes)
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

parseTcmStream :: Stream s m Char => s -> m (Either ParseError TCM)
parseTcmStream = runParserT (validateParseResult =<< tcmDefinition <* eof) () "Custom alphabet and its associated cost matrix"

tcmDefinition :: Stream s m Char => ParsecT s u m ParseResult
tcmDefinition = do
    _        <- spaces
    alphabet <- symbol alphabetLine
    matrix   <- symbol matrixBlock
    pure $ ParseResult alphabet matrix
  where
    alphabetLine   = (alphabetSymbol <* inlineSpaces) `manyTill` eol
    alphabetSymbol = many1 nonSpace
    nonSpace       = satisfy (not . isSpace)
    matrixBlock    = many (symbol matrixRow)
    matrixRow      = (matrixEntry <* inlineSpaces) `manyTill` eol
    matrixEntry    = decimal

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
    doubleAlphabet = case dupes alphabet of
                       [] -> Nothing
                       xs -> Just $ "The following symbols were listed multiple times in the custom alphabet: " ++ show xs
    emptyMatrix    = case protoMatrix of
                       [] -> Just "No matrix specified"
                       _  -> Nothing
    badRowCount    = if   rows /= size 
                     then Just $ "Matrix has "++show rows++" rows but "++show size++" rows were expected"
                     else Nothing
    badColCount    = foldr g [] badCols

dupes :: Ord a => [a] -> [a]
dupes = dupes' . sort
  where
    dupes'       [] = []
    dupes'      [_] = []
    dupes' (x:y:ys) = if   x == y 
                      then x : dupes' (dropWhile (y==) ys) 
                      else dupes' (y:ys)

symbol  :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
symbol  x = x <* spaces
