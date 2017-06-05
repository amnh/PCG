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


data OperationalMode a
   = Search
   | SearchAgainst    a
   | RenderComparison a a
   | TooManyParameters
   

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Search                       -> performCounterExampleSearch Nothing
      SearchAgainst    char1       -> performCounterExampleSearch $ Just char1
      RenderComparison char1 char2 -> performImplementationComparison char1 char2
      TooManyParameters            -> putStrLn "Expecting only two parameters!"


parseArgs :: [String] -> OperationalMode DynamicChar -- Either String (String, String)
parseArgs args =
    case args of
      []          -> Search
      arg1:xs     ->
        let char1 = readSequence arg1
        in  case xs of
               []     -> SearchAgainst    char1
               [arg2] -> RenderComparison char1 (readSequence arg2)
               _      -> TooManyParameters 
  where
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList


performCounterExampleSearch :: Maybe DynamicChar -> IO ()
performCounterExampleSearch valueMay = do 
    putStrLn "Performing stocastic counter-example search:"
    case valueMay of
      Nothing   -> quickCheckWith stdArgs { maxSuccess = 10000 } $ uncurry counterExampleCheck
      Just char -> quickCheckWith stdArgs { maxSuccess = 10000 } $ counterExampleCheck (NS char)
-- Can't do this because the monomorphism restriction unexpectedly fixes the type.
--  where
--    stocasticSearchContext = quickCheckWith stdArgs { maxSuccess = 10000 }


counterExampleCheck :: NucleotideSequence -> NucleotideSequence -> Bool
counterExampleCheck (NS lhs) (NS rhs) = all (== naiveDOResult) [memoizeDOResult, ukkonenDOResult, foreignDOResult]
  where
    naiveDOResult    = naiveDO           lhs rhs  costStructure
    memoizeDOResult  = naiveDOMemo       lhs rhs (getMedianAndCost memoMatrixValue)
    ukkonenDOResult  = ukkonenDO         lhs rhs (getMedianAndCost memoMatrixValue)
    foreignDOResult  = foreignPairwiseDO lhs rhs  denseMatrixValue
--    shownInputs      = mconcat ["\n(",showStream alphabet lhs,",",showStream alphabet rhs,")"]


performImplementationComparison :: DynamicChar -> DynamicChar -> IO ()
performImplementationComparison char1 char2 = do
    putStrLn "Native Naive    DO Result:"
    putStrLn   naiveMessage
    putStrLn "Native Memoized DO Result:"
    putStrLn memoizeMessage
    putStrLn "Native Ukkonen  DO Result:"
    putStrLn ukkonenMessage
    putStrLn "Foreign C code  DO Result:"
    putStrLn foreignMessage
    if   all (== naiveMessage) [memoizeMessage, ukkonenMessage, foreignMessage]
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    naiveMessage     = renderResult   naiveDOResult
    memoizeMessage   = renderResult memoizeDOResult
    ukkonenMessage   = renderResult ukkonenDOResult
    foreignMessage   = renderResult foreignDOResult
    naiveDOResult    = naiveDO           char1 char2  costStructure
    memoizeDOResult  = naiveDOMemo       char1 char2 (getMedianAndCost memoMatrixValue)
    ukkonenDOResult  = ukkonenDO         char1 char2 (getMedianAndCost memoMatrixValue)
    foreignDOResult  = foreignPairwiseDO char1 char2  denseMatrixValue
--    char1 = readSequence lhs
--    char2 = readSequence rhs
--    readSequence :: String -> DynamicChar
--    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
    renderResult (c, w, x, y, z) = unlines
        [ "  Cost           : " <> show c 
        , "  Median ungapped: " <> showStream alphabet w
        , "  Median   gapped: " <> showStream alphabet x
        , "  LHS   alignment: " <> showStream alphabet y
        , "  RHS   alignment: " <> showStream alphabet z
        ]


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


costStructure :: (Ord a, Num a) => a -> a -> a
costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
