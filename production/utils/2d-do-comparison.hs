{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup
import           Data.TCM.Memoized
import           System.Environment        (getArgs)
import           Test.Custom.NucleotideSequence
import           Test.QuickCheck


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Left          _ -> performCounterExampleSearch
      Right (lhs,rhs) -> performImplementationComparison lhs rhs


parseArgs :: [String] -> Either String (String, String)
parseArgs args =
  case args of
    []          -> Left "No arguments supplied!"
    [_]         -> Left "Only one argument supplied, expecting two sequences."
    arg1:arg2:_ -> Right (arg1, arg2)


performCounterExampleSearch :: IO ()
performCounterExampleSearch = do 
    putStrLn "Performing stocastic counter-example search:"
    quickCheckWith stdArgs { maxSuccess = 10000 } counterExampleCheck


counterExampleCheck :: (NucleotideSequence, NucleotideSequence) -> Bool
counterExampleCheck (NS lhs, NS rhs) =
    nativeDOResult == foreignDOResult && foreignDOResult == ukkonenDOResult
  where
    nativeDOResult  = naiveDOMemo       lhs rhs (getMedianAndCost memoMatrixValue)
    ukkonenDOResult = ukkonenDO         lhs rhs  undefined -- (getMedianAndCost memoMatrixValue)
    foreignDOResult = foreignPairwiseDO lhs rhs  denseMatrixValue


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = do
    putStrLn "Native DO Result:"
    putStrLn nativeMessage
    putStrLn "Foreign DO Result:"
    putStrLn foreignMessage
    if   nativeMessage == foreignMessage && foreignMessage == ukkonenMessage
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    nativeMessage    = renderResult  nativeDOResult
    ukkonenMessage   = renderResult ukkonenDOResult
    foreignMessage   = renderResult foreignDOResult
    nativeDOResult   = naiveDOMemo       char1 char2 (getMedianAndCost memoMatrixValue)
    ukkonenDOResult  = ukkonenDO         char1 char2  undefined -- (getMedianAndCost memoMatrixValue)
    foreignDOResult  = foreignPairwiseDO char1 char2  denseMatrixValue
    char1 = readSequence lhs
    char2 = readSequence rhs
    alphabet = fromSymbols ["A","C","G","T"]
    readSequence :: String -> DynamicChar
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
    renderResult (c, w, x, y, z) = unlines
        [ "Cost           : " <> show c 
        , "Median ungapped: " <> showStream alphabet w
        , "Median   gapped: " <> showStream alphabet x
        , "LHS   alignment: " <> showStream alphabet y
        , "RHS   alignment: " <> showStream alphabet z
        ]


costStructure :: (Ord a, Num a) => a -> a -> a
--costStructure i j = if i /= j then 1 else 0
costStructure i j = max i j - min i j


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
