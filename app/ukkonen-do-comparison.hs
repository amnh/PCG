{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                                    as BM
import qualified Data.List.NonEmpty                            as NE
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           System.Environment                            (getArgs)
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
    putStrLn "Performing stochastic counter-example search:"
    quickCheckWith stdArgs { maxSuccess = 10000000 } counterExampleCheck


counterExampleCheck :: (NucleotideSequence, NucleotideSequence) -> Bool
counterExampleCheck (NS lhs, NS rhs) =
    ukkonenDOResult == foreignDOResult
  where
    ukkonenDOResult = ukkonenDO         (getMedianAndCost2D memoMatrixValue) lhs rhs
    foreignDOResult = foreignPairwiseDO denseMatrixValue  lhs rhs


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = do
    putStrLn "Ukkonen DO Result:"
    putStrLn ukkonenMessage
    putStrLn "Foreign DO Result:"
    putStrLn foreignMessage
    if   ukkonenMessage == foreignMessage
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    ukkonenMessage   = renderResult ukkonenDOResult
    foreignMessage   = renderResult foreignDOResult
    ukkonenDOResult  = ukkonenDO         tcm char1 char2
    foreignDOResult  = foreignPairwiseDO denseMatrixValue char1 char2
    tcm      = getMedianAndCost2D memoMatrixValue
    tcm' x y = fst $ tcm x y
    char1    = readSequence lhs
    char2    = readSequence rhs
    alphabet = fromSymbols ["A","C","G","T"]
    readSequence :: String -> DynamicCharacter
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
    renderResult (c, w) = unlines
        [ "Cost     : " <> show c
        , "Alignment: " <> renderDynamicCharacter alphabet tcm' w
        ]


costStructure :: (Ord a, Num a) => a -> a -> a
costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
