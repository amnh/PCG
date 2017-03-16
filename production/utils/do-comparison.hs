{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import qualified Data.Bimap         as BM
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup
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
    nativeDOResult  = naiveDO           lhs rhs costStructure
    foreignDOResult = foreignPairwiseDO lhs rhs matrixValue
    matrixValue     = generateDenseTransitionCostMatrix 5 costStructure
    costStructure i j = if i /= j then 1 else 0


performImplementationComparison :: String -> String -> IO ()
performImplementationComparison lhs rhs = do
    putStrLn "Native DO Result:"
    putStrLn nativeMessage
    putStrLn "Foreign DO Result:"
    putStrLn foreignMessage
    if   nativeMessage == foreignMessage
    then putStrLn "[!] Results MATCH"
    else putStrLn "[X] Results DO NOT MATCH"
  where
    nativeMessage   = renderResult nativeDOResult
    foreignMessage  = renderResult foreignDOResult
    nativeDOResult  = naiveDO           char1 char2 costStructure
    foreignDOResult = foreignPairwiseDO char1 char2 matrixValue
    matrixValue     = generateDenseTransitionCostMatrix 5 costStructure
    char1 = readSequence lhs
    char2 = readSequence rhs
    alphabet = fromSymbols ["A","C","G","T"]
    costStructure i j = if i /= j then 1 else 0
    readSequence :: String -> DynamicChar
    readSequence = encodeStream alphabet . fmap ((iupacToDna BM.!) . pure . pure) . NE.fromList
    renderResult (w, c, x, y, z) = unlines
        [ "Cost           : " <> show c 
        , "Median ungapped: " <> showStream alphabet w
        , "Median   gapped: " <> showStream alphabet x
        , "LHS   alignment: " <> showStream alphabet y
        , "RHS   alignment: " <> showStream alphabet z
        ]

{-
readFiles :: (FilePath, FilePath) -> IO (FilePath, String, FilePath, String)
readFiles (path1, path2) = do
    content1 <- readFile path1
    content2 <- readFile path2
    pure (path1, content1, path2, content2)

parseFiles :: (FilePath, String, FilePath, String) -> Either String ([FastaSequence], [FastaSequence])
parseFiles (path1, file1, path2, file2) = liftA2 ((,)) (parse' path1 file1) (parse' path2 file2)
  where
    parse' path stream = first parseErrorPretty (parse fastaStreamParser path stream :: Either (ParseError Char Dec) [FastaSequence])

performFileDiff :: (FastaParseResult, FastaParseResult) -> Either String String
performFileDiff (lhs, rhs) = maybe (Right fileDiffResult) Left errorMessage 
  where
    fileDiffResult = foldMapWithKey renderTaxa sequenceUnion
    sequenceUnion  = unionWith (\x y -> unlines [x,y]) lhsMap rhsMap
    lhsMap         = toMap lhs
    rhsMap         = toMap rhs
    lhsKeys        = Set.fromList $ keys lhsMap
    rhsKeys        = Set.fromList $ keys rhsMap

    toMap :: FastaParseResult -> Map String String
    toMap = foldMap (singleton <$> taxonName <*> taxonSequence)

    renderTaxa :: String -> String -> String
    renderTaxa taxaName taxaSequences = mconcat [ "> ", taxaName, "\n", taxaSequences, "\n"] 
    
    errorMessage
      | null lhsUnique &&
        null rhsUnique = Nothing
      | otherwise      = Just $ unlines
          [ "The taxa from the two files do not exacly match!"
          , "Found the following unique taxa in the first file:"
          , show lhsUnique
          , "Found the following unique taxa in the second file:"
          , show rhsUnique
          ]
      where
        intersected = lhsKeys `intersection` rhsKeys
        lhsUnique   = lhsKeys `difference` intersected
        rhsUnique   = rhsKeys `difference` intersected
-}
                 
