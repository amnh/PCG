{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap                                    as BM
import           Data.Foldable
import           Data.List                                     (isPrefixOf, uncons)
import           Data.List.NonEmpty                            (NonEmpty)
import qualified Data.List.NonEmpty                            as NE
import           Data.Maybe
import           Data.MonoTraversable
--import           Data.TCM.Dense
import           Data.TCM.Memoized
import           System.Environment                            (getArgs)
import           Test.Custom.NucleotideSequence
import           Test.Tasty
import           Test.Tasty.QuickCheck


data OperationalMode a
   = Search
   | SearchAgainst    a
   | RenderComparison a a
   | TooManyParameters


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Search                       -> performCounterExampleSearch' Nothing
      SearchAgainst    char1       -> performCounterExampleSearch' $ Just char1
      RenderComparison char1 char2 -> performImplementationComparison char1 char2
      TooManyParameters            -> putStrLn "Expecting only two parameters!"


parseArgs :: [String] -> OperationalMode DynamicCharacter
parseArgs args =
    case args of
      []          -> Search
      arg1:_ | "--quickcheck-replay" `isPrefixOf` arg1 -> Search
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


performCounterExampleSearch' :: Maybe DynamicCharacter -> IO ()
performCounterExampleSearch' valueMay =
        case valueMay of
          Nothing   -> makeMain . testProperty preamble $ uncurry counterExampleCheck
          Just char -> makeMain . testProperty preamble $ counterExampleCheck (NS char)
  where
    preamble = "Performing stocastic counter-example search"
    makeMain = defaultMain . localOption (QuickCheckTests 1000000) . localOption (QuickCheckShowReplay True)


counterExampleCheck :: NucleotideSequence -> NucleotideSequence -> Property
counterExampleCheck (NS lhs) (NS rhs) = uncurry counterexample $ gatherContexts lhs rhs


performImplementationComparison :: DynamicCharacter -> DynamicCharacter -> IO ()
performImplementationComparison lhs rhs = putStrLn . fst $ gatherContexts lhs rhs


gatherContexts
  :: DynamicCharacter
  -> DynamicCharacter
  -> (String, Bool)
gatherContexts lhs rhs = (contextRendering, contextSameness)
  where
    contextSameness  = sameAlignment $ snd <$> contexts

    contextRendering = renderContexts (NS lhs) (NS rhs) contexts

    contexts =
        [ ("Old Full"    ,        memoizeDOResult)
        , ("Old Ukkonen" ,     ukkonenOldDOResult)
        , ("Unboxed Full",        unboxedDOResult)
        , ("Unboxed Swapping",   swappingDOResult)
        , ("Unboxed Ukkonen",  ukkonenNewDOResult)
        ]

--    naiveDOResult      = naiveDO             lhs rhs costStructure
--    foreignDOResult    = foreignPairwiseDO   lhs rhs denseMatrixValue
    memoizeDOResult    = naiveDOMemo         lhs rhs tcm
    ukkonenOldDOResult = ukkonenDO           lhs rhs tcm
    unboxedDOResult    = unboxedFullMatrixDO lhs rhs tcm
    swappingDOResult   = unboxedSwappingDO   lhs rhs tcm
    ukkonenNewDOResult = unboxedUkkonenDO    lhs rhs tcm


sameAlignment :: (Foldable t, MonoFoldable s, Eq c, Eq s) => t (c, s) -> Bool
sameAlignment v =
    case uncons $ toList v of
      Nothing       -> True
      Just ((c,a),xs) ->
          let sameCost = all (== c)         $           fst <$> xs
              sameLen  = all (== olength a) $ olength . snd <$> xs
              sameStr  = all (== a)         $           snd <$> xs
          in  sameCost && (not sameLen || sameStr)


renderContexts
 :: ( Eq c
    , Foldable f
    , Functor f
    , Show c
    )
 => NucleotideSequence
 -> NucleotideSequence
 -> f (String, (c, DynamicCharacter))
 -> String
renderContexts m n xs = unlines . (\x -> [prefix] <> x <> [suffix]) . fmap f $ toList xs
  where
    f (s, c) = s <> "\n" <> renderResult c
    renderResult (cost, aligned) = unlines
        [ "  Cost     : " <> show cost
        , "  Alignment: " <> renderDynamicCharacter alphabet tcm' aligned
        ]

    prefix = show (m, n)
    suffix
      | sameAlignment $ snd <$> xs = "[!] Results MATCH"
      | otherwise                  = "[X] Results DO NOT MATCH"

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


--denseMatrixValue :: DenseTransitionCostMatrix
--denseMatrixValue = generateDenseTransitionCostMatrix 0  5 costStructure


memoMatrixValue :: MemoizedCostMatrix
memoMatrixValue  = generateMemoizedTransitionCostMatrix 5 costStructure
