{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                                    as BM
import           Data.List.NonEmpty                            (NonEmpty)
import qualified Data.List.NonEmpty                            as NE
import           Data.Maybe
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           System.Environment                            (getArgs)
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


parseArgs :: [String] -> OperationalMode DynamicCharacter -- Either String (String, String)
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
    readSequence = encodeStream alphabet . fmap ((iupacToDna .!.) . pure . pure) . NE.fromList

    (.!.) :: BM.Bimap (NonEmpty String) (NonEmpty String)
          -> NonEmpty String
          -> NonEmpty String
    (.!.) bm k = fromMaybe (error $ "Bimap left key not found: " <> show k) $ BM.lookup k bm


performCounterExampleSearch :: Maybe DynamicCharacter -> IO ()
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
counterExampleCheck x@(NS lhs) y@(NS rhs) = counterexample contextRendering $
--    all (== naiveDOResult) [memoizeDOResult, ukkonenDOResult, foreignDOResult]
--    all (== ukkonenDOResult) [ukkonenDOResult]
    all (== memoizeDOResult) [memoizeDOResult, ukkonenDOResult]
  where
    contextRendering = niceContextRendering x y naiveDOResult memoizeDOResult ukkonenDOResult foreignDOResult
--    contextRendering = niceContextRendering x y memoizeDOResult ukkonenDOResult foreignDOResult
--    contextRendering = niceContextRendering x y ukkonenDOResult
    naiveDOResult    = naiveDO           lhs rhs costStructure
    memoizeDOResult  = naiveDOMemo       lhs rhs tcm
    ukkonenDOResult  = ukkonenDO         lhs rhs tcm
    foreignDOResult  = foreignPairwiseDO lhs rhs denseMatrixValue
--    shownInputs      = mconcat ["\n(",showStream alphabet lhs,",",showStream alphabet rhs,")"]


performImplementationComparison :: DynamicCharacter -> DynamicCharacter -> IO ()
performImplementationComparison char1 char2 = putStrLn renderedComparison
  where
    renderedComparison = niceContextRendering (NS char1) (NS char2) naiveDOResult memoizeDOResult ukkonenDOResult foreignDOResult
    naiveDOResult      = naiveDO           char1 char2 costStructure
    memoizeDOResult    = naiveDOMemo       char1 char2 tcm
    ukkonenDOResult    = ukkonenDO         char1 char2 tcm
    foreignDOResult    = foreignPairwiseDO char1 char2 denseMatrixValue


niceContextRendering
  :: ( Show c1
     , Show c2
     , Show c3
     , Show c4
     )
  => NucleotideSequence
  -> NucleotideSequence
  -> (c1, DynamicCharacter)
  -> (c2, DynamicCharacter)
  -> (c3, DynamicCharacter)
  -> (c4, DynamicCharacter)
  -> String
niceContextRendering m n a b c d = unlines
    [ show (m, n)
    , "Native Naive    DO Result:"
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
    renderResult (cost, aligned) = unlines
        [ "  Cost     : " <> show cost
        , "  Alignment: " <> renderDynamicCharacter alphabet tcm' aligned
        ]

    whichDoNotMatch =
        case messageDiffs of
          [] -> "[!] Results MATCH"
          xs -> "[X] Results DO NOT MATCH\n\n" <> foldMap renderDiff xs
      where
        messages     = [ {- naiveMessage, memoizeMessage, -} ukkonenMessage {- , foreignMessage -}]
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


tcm :: AmbiguityGroup -> AmbiguityGroup -> (AmbiguityGroup, Word)
tcm = getMedianAndCost2D memoMatrixValue


tcm' :: AmbiguityGroup -> AmbiguityGroup -> AmbiguityGroup
tcm' x y = fst $ tcm x y
               

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
