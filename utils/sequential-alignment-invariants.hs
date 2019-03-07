{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
import           Analysis.Parsimony.Dynamic.SequentialAlign
import           Bio.Character.Encodable
import           Bio.TCM.Memoized
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                                             as BM
import qualified Data.List.NonEmpty                                     as NE
import           System.Environment                                     (getArgs)
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
counterExampleCheck (NS lhs, NS rhs) = lhs == filterGaps lhs'
                                    && rhs == filterGaps rhs'
  where
    (_,_,_,lhs',rhs') = sequentialAlign tcm lhs rhs


tcm :: MemoizedCostMatrix
tcm = generateMemoizedTransitionCostMatrix (toEnum $ length alphabet) costStructure
  where
    costStructure i j = if i /= j then 1 else 0


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison str1 str2 = do
    putStrLn $ unlines
        [ "Left  hand side:"
        , "  Input : " <> showCharacter lhs
        , "  Output: " <> showCharacter lhs'
        , "Right hand side:"
        , "  Input : " <> showCharacter rhs
        , "  Output: " <> showCharacter rhs'
        ]
    if lhs == filterGaps lhs' && rhs == filterGaps rhs'
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    (_,_,_,lhs',rhs') = sequentialAlign tcm lhs rhs
    lhs = readSequence str1
    rhs = readSequence str2
    showCharacter = showStream alphabet
    readSequence :: String -> DynamicCharacter
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
