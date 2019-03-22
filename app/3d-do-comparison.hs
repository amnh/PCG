{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Control.DeepSeq
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
    putStrLn "Performing stocastic counter-example search:"
    quickCheckWith stdArgs { maxSuccess = 10000 } counterExampleCheck


counterExampleCheck :: (NucleotideSequence, NucleotideSequence) -> Property
counterExampleCheck (NS lhs, NS rhs) = counterexample renderedExample $
    nativeDOResult == foreignDOResult
  where
    renderedExample = niceContextRendering nativeDOResult foreignDOResult
    nativeDOResult  =         naiveDOMemo       lhs rhs (getMedianAndCost2D memoMatrixValue)
    foreignDOResult = chomp $ foreignThreeWayDO lhs rhs rhs 1 1 1 denseMatrixValue


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = putStrLn renderedComparison
  where
    char1    = readSequence lhs
    char2    = readSequence rhs

    readSequence :: String -> DynamicCharacter
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList

    renderedComparison = niceContextRendering nativeDOResult foreignDOResult
    nativeDOResult     =         naiveDOMemo       char1 char2 (getMedianAndCost2D memoMatrixValue)
    foreignDOResult    = chomp $ foreignThreeWayDO char1 char2 char2 1 1 1 denseMatrixValue


niceContextRendering
  :: ( EncodableStream s1
     , EncodableStream s2
     , EncodableStream s3
     , EncodableStream s4
     , EncodableStream s5
     , EncodableStream s6
     , EncodableStream s7
     , EncodableStream s8
     , NFData c1
     , NFData c2
     , NFData s1
     , NFData s2
     , NFData s3
     , NFData s4
     , NFData s5
     , NFData s6
     , NFData s7
     , NFData s8
     , Show c1
     , Show c2
     )
  => (c1, s1, s2, s3, s4)
  -> (c2, s5, s6, s7, s8)
  -> String
niceContextRendering a b = unlines
    [ "Native  2D DO Result:"
    , nativeMessage
    , "Foreign 3D DO Result:"
    , foreignMessage
    , whichDoNotMatch
{-
    , if   all (== naiveMessage) [memoizeMessage, ukkonenMessage, foreignMessage]
      then "[!] Results MATCH"
      else "[X] Results DO NOT MATCH"
-}
    ]
  where
    nativeMessage  = renderResult $ force a
    foreignMessage = renderResult $ force b
    renderResult (v, w, x, y, z) = unlines
        [ "  Cost           : " <> show v
        , "  Median ungapped: " <> showStream alphabet w
        , "  Median   gapped: " <> showStream alphabet x
        , "  LHS   alignment: " <> showStream alphabet y
        , "  RHS   alignment: " <> showStream alphabet z
        ]

    whichDoNotMatch =
        case diffIndex nativeMessage foreignMessage of
          Nothing -> "[!] Results MATCH"
          Just i  -> unwords [ "[X] Results DO NOT MATCH ( at string index ", show i, ")" ]


diffIndex :: String -> String -> Maybe Int
diffIndex x y
  | null remaining = Nothing
  | otherwise      = Just $ length matched
  where
    (matched, remaining) = span id $ zipWith (==) x y


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


chomp :: (a,b,c,d,e,f) -> (a,b,c,d,e)
chomp (a,b,c,d,e,_) = (a,b,c,d,e)


costStructure :: (Ord a, Num a) => a -> a -> a
costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
