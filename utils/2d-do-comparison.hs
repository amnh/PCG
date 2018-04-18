{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
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
      Nothing   -> quickCheckWith extraItertations $ uncurry counterExampleCheck
      Just char -> quickCheckWith extraItertations $ counterExampleCheck (NS char)
  where
    extraItertations = stdArgs { maxSuccess = 1000000 }
-- Can't do this because the monomorphism restriction unexpectedly fixes the type.
--  where
--    stocasticSearchContext = quickCheckWith stdArgs { maxSuccess = 10000 }


counterExampleCheck :: NucleotideSequence -> NucleotideSequence -> Property
counterExampleCheck (NS lhs) (NS rhs) = counterexample contextRendering $
    all (== naiveDOResult) [memoizeDOResult, ukkonenDOResult, foreignDOResult]
  where
    contextRendering = niceContextRendering naiveDOResult memoizeDOResult ukkonenDOResult foreignDOResult
    naiveDOResult    = naiveDO           lhs rhs  costStructure
    memoizeDOResult  = naiveDOMemo       lhs rhs (getMedianAndCost2D memoMatrixValue)
    ukkonenDOResult  = ukkonenDO         lhs rhs (getMedianAndCost2D memoMatrixValue)
    foreignDOResult  = foreignPairwiseDO lhs rhs  denseMatrixValue
--    shownInputs      = mconcat ["\n(",showStream alphabet lhs,",",showStream alphabet rhs,")"]


performImplementationComparison :: DynamicChar -> DynamicChar -> IO ()
performImplementationComparison char1 char2 = putStrLn renderedComparison
  where
    renderedComparison = niceContextRendering naiveDOResult memoizeDOResult ukkonenDOResult foreignDOResult
    naiveDOResult      = naiveDO           char1 char2  costStructure
    memoizeDOResult    = naiveDOMemo       char1 char2 (getMedianAndCost2D memoMatrixValue)
    ukkonenDOResult    = ukkonenDO         char1 char2 (getMedianAndCost2D memoMatrixValue)
    foreignDOResult    = foreignPairwiseDO char1 char2  denseMatrixValue


niceContextRendering 
  :: ( Show c1
     , Show c2
     , Show c3
     , Show c4
     , EncodableStream s1
     , EncodableStream s2
     , EncodableStream s3
     , EncodableStream s4
     , EncodableStream s5
     , EncodableStream s6
     , EncodableStream s7
     , EncodableStream s8
     , EncodableStream s9
     , EncodableStream s10
     , EncodableStream s11
     , EncodableStream s12
     , EncodableStream s13
     , EncodableStream s14
     , EncodableStream s15
     , EncodableStream s16
     )
  => (c1, s1 , s2 , s3 , s4 )
  -> (c2, s5 , s6 , s7 , s8 )
  -> (c3, s9 , s10, s11, s12)
  -> (c4, s13, s14, s15, s16)
  -> String
niceContextRendering a b c d = unlines
    [ "Native Naive    DO Result:"
    , naiveMessage
    , "Native Memoized DO Result:"
    , memoizeMessage
    , "Native Ukkonen  DO Result:"
    , ukkonenMessage
    , "Foreign C code  DO Result:"
    , foreignMessage
    , whichDoNotMatch
{-
    , if   all (== naiveMessage) [memoizeMessage, ukkonenMessage, foreignMessage]
      then "[!] Results MATCH"
      else "[X] Results DO NOT MATCH"
-}
    ]
  where
    naiveMessage   = renderResult a
    memoizeMessage = renderResult b
    ukkonenMessage = renderResult c
    foreignMessage = renderResult d
    renderResult (v, w, x, y, z) = unlines
        [ "  Cost           : " <> show v
        , "  Median ungapped: " <> showStream alphabet w
        , "  Median   gapped: " <> showStream alphabet x
        , "  LHS   alignment: " <> showStream alphabet y
        , "  RHS   alignment: " <> showStream alphabet z
        ]

    whichDoNotMatch =
        case messageDiffs of
          [] -> "[!] Results MATCH"
          xs -> "[X] Results DO NOT MATCH\n\n" <> foldMap renderDiff xs
      where
        messages     = [naiveMessage, memoizeMessage, ukkonenMessage, foreignMessage]
        messageDiffs = catMaybes [ (\i -> (i, x, y)) <$> diffIndex x y | x <- messages, y <- messages, x > y ]
        renderDiff (i, x, y) = unlines [ "At index " <> show i, x, y]


diffIndex :: String -> String -> Maybe Int
diffIndex x y
  | null remaining = Nothing
  | otherwise      = Just $ length matched
  where
    (matched, remaining) = span id $ zipWith (==) x y


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]


costStructure :: (Ord a, Num a) => a -> a -> a
--costStructure i j = if i /= j then 1 else 0
--costStructure i j = max i j - min i j
costStructure i j
  | i == j    = 0
  | i == 4    = 1
  | j == 4    = 1
  | otherwise = 2


denseMatrixValue :: DenseTransitionCostMatrix
denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
