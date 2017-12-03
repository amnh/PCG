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
counterExampleCheck (NS lhs, NS rhs) = nativeDOResult == foreignDOResult
  where
    nativeDOResult  =         naiveDOMemo       lhs rhs (getMedianAndCost memoMatrixValue)
    foreignDOResult = chomp $ foreignThreeWayDO lhs rhs rhs 1 1 1 denseMatrixValue


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = do
    putStrLn "Native  2D DO Result:"
    putStrLn nativeMessage
    putStrLn "Foreign 3D DO Result:"
    putStrLn foreignMessage
    if   nativeMessage == foreignMessage
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    nativeMessage    = renderResult nativeDOResult
    foreignMessage   = renderResult foreignDOResult
    nativeDOResult   =         naiveDOMemo       char1 char2 (getMedianAndCost memoMatrixValue)
    foreignDOResult  = chomp $ foreignThreeWayDO char1 char2 char2 1 1 1 denseMatrixValue
    char1 = readSequence lhs
    char2 = readSequence rhs
    alphabet = fromSymbols ["A","C","G","T"]
    readSequence :: String -> DynamicChar
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
    renderResult  (c, w, x, y, z) = unlines
        [ "Cost           : " <> show c
        , "Median ungapped: " <> showStream alphabet w
        , "Median   gapped: " <> showStream alphabet x
        , "LHS   alignment: " <> showStream alphabet y
        , "RHS   alignment: " <> showStream alphabet z
        ]


chomp :: (a,b,c,d,e,f) -> (a,b,c,d,e)
chomp (a,b,c,d,e,_) = (a,b,c,d,e)

costStructure :: (Ord a, Num a) => a -> a -> a
costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure