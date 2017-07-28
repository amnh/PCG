{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import           Control.Applicative (liftA2)
import           Data.Bifunctor      (first)
import           Data.Key
import           Data.Map            (Map, keys, singleton, unionWith)
import           Data.Set            (difference, intersection)
import qualified Data.Set     as Set (fromList)
import           Data.Void
import           File.Format.Fasta
import           System.Environment  (getArgs)
import           Text.Megaparsec    -- (parse, parseErrorPretty)

main :: IO ()
main = do
   args <- getArgs
   case parseArgs args of
     Left  argsError -> print argsError
     Right filePaths -> do
       fileData <- readFiles filePaths
       case performFileDiff =<< parseFiles fileData of
         Left  parseError   -> putStr parseError
         Right resultStream -> putStr resultStream


parseArgs :: [String] -> Either String (FilePath, FilePath)
parseArgs args =
  case args of
    []          -> Left "No arguments supplied!"
    [_]         -> Left "Only one argument supplied, expecting two files."
    arg1:arg2:_ -> Right (arg1, arg2)


readFiles :: (FilePath, FilePath) -> IO (FilePath, String, FilePath, String)
readFiles (path1, path2) = do
    content1 <- readFile path1
    content2 <- readFile path2
    pure (path1, content1, path2, content2)


parseFiles :: (FilePath, String, FilePath, String) -> Either String ([FastaSequence], [FastaSequence])
parseFiles (path1, file1, path2, file2) = liftA2 (,) (parse' path1 file1) (parse' path2 file2)
  where
    parse' path stream = first parseErrorPretty (parse fastaStreamParser path stream :: Either (ParseError Char Void) [FastaSequence])


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
      | otherwise      = Just $ mconcat
                              [ "The taxa from the two files do not exacly match!\n"
                              , "Found the following unique taxa in the first file\n:"
                              , show lhsUnique
                              , "Found the following unique taxa in the second file:\n"
                              , show rhsUnique
                              ]
      where
        intersected = lhsKeys `intersection` rhsKeys
        lhsUnique   = lhsKeys `difference` intersected
        rhsUnique   = rhsKeys `difference` intersected

                 
