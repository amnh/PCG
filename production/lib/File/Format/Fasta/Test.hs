{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module File.Format.Fasta.Test
  ( testSuite
  ) where

import Control.Arrow ((&&&))
import Data.Char                  (isSpace)
import Data.Map                   (Map,fromList,toList)
import File.Format.Fasta.Internal
import File.Format.Fasta.Parser
import Safe                       (headMay)
import System.Directory
-- | In order to build an informative test tree dynamically
--   with informatin derived from the file system we need to
--   use unsafe IO operation when building the subtree. Specifically,
--   we coerce IO [TestTree] -> [TestTree]
--import System.IO.Unsafe           (unsafePerformIO)
import Test.Tasty                 (TestTree,testGroup,withResource)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec                (parse,eof)

testSuite :: TestTree
testSuite = testGroup "Fasta Format"
  [ testGroup "Fasta Generalized Combinators" [identifier',commentBody',identifierLine']
  , testGroup "Fasta Parser" [fastaSequence',validFastaFiles]
  ]

identifier' :: TestTree
identifier' = testGroup "identifier" $ [invariant, valid, invalid]
  where
    valid         = testGroup "Valid taxon labels"   $ success <$>   validTaxonLabels
    invalid       = testGroup "Invalid taxon labels" $ failure <$> invalidTaxonLabels
    success str   = testCase (show str) . assert $ parse' str == Right str
    failure str   = testCase (show str) . assert . isLeft $ parse' str
    parse' = parse (identifier <* eof) ""
    invalidTaxonLabels =
      [ x ++ [s] ++ y |  e    <- validTaxonLabels
                      ,  s    <- "$ \t\r\n"
                      ,  i    <- [length e `div` 2]
                      , (x,y) <- [i `splitAt` e] 
      ]
    invariant = testProperty "fastaLabel invariant" f
      where 
        f x = null str || parse identifier "" str == Right res
          where
            str = takeWhile validIdentifierChar x
            res = headOrEmpty $ words str

validTaxonLabels :: [String]
validTaxonLabels = 
  [ "Peripatidae"
  , "Colossendeis"
  , "Ammotheidae"
  , "Buthidae"
  , "Mygalomorphae"
  ]

commentBody' :: TestTree
commentBody' = testGroup "commentBody" [generalComment, prependedDollarSign, validComments]
  where
    generalComment :: TestTree
    generalComment = testProperty "General comment structure" f
      where
        f x = hasLeadingDollarSign 
           || null res
           || parse commentBody "" x == Right res
          where
            res = unwords . words $ takeWhile (not.(`elem`"\n\r")) x
            hasLeadingDollarSign = let y = dropWhile isSpace x
                                   in  headMay y == Just '$'
    prependedDollarSign :: TestTree
    prependedDollarSign = testProperty "Comments defined with leading dollar sign" $ prepended '$'
      where
        prepended :: Char -> String -> Bool
        prepended c x = null x || null line'
                     || ( parse commentBody "" (            c   : line) == Right line'
                       && parse commentBody "" (      ' ' : c   : line) == Right line'
                       && parse commentBody "" (      c   : ' ' : line) == Right line'
                       && parse commentBody "" (' ' : c   : ' ' : line) == Right line'
                        )
          where
            line  = takeWhile (not . (`elem`"\n\r")) x
            line' = unwords $ words line
    validComments = testGroup "Valid comments" $ success <$> validCommentBodies
      where 
        success str = testCase (show str) . assert $ parse' str == Right "A species of animal"
        parse' = parse (commentBody <* eof) ""

validCommentBodies :: [String]
validCommentBodies =
  [ "$A species of animal"
  , " $A species of animal"
  , "$ A species of animal"
  , " $ A species of animal"
  , " A species of animal"
  ]

identifierLine' :: TestTree
identifierLine' = testGroup "fastaLabelLine" $ [validWithoutComments, validWithComments]
  where
    validWithoutComments = testGroup "Valid taxon label lines without comemnts" $ success <$> validTaxonCommentlessLines
    validWithComments    = testGroup "Valid taxon label lines with comments"    $ success <$> validTaxonCommentLines
    success (str,res) = testCase (show str) . assert $ parse' str == Right res
    parse' = parse (identifierLine <* eof) ""
    validTaxonCommentLines     = zip validTaxonCommentedLabels validTaxonLabels
    validTaxonCommentedLabels  = (\x -> "> "++x++"\n") <$> zipWith (++) validTaxonLabels validCommentBodies
    validTaxonCommentlessLines = zip ((\x -> "> "++x++"\n") <$> validTaxonLabels) validTaxonLabels

fastaSequence' :: TestTree
fastaSequence' = testGroup "fastaNucleotides" $ [valid]
  where
    parse' = parse fastaSequence ""
    success str = testCase (show str) . assert $ parse' str == Right "-GATACA-"
    valid = testGroup "Valid sequences" $ success <$> validSequences
    validSequences =
      [ "-GATACA-\n"
      , "- G ATA CA- \n"
      , "-GAT\nACA-\n"
      , " -G A\nT\n AC A- \n"
      , "-GA\n\nT\n \nACA-\n"
      ]


-- | Reads in all files in a directory and ensures that they correctly parse
--   NOTE: Library hack :(
--   On success, no file names will be displayed.
--   On the first failure, no subsequent files will have parsing attempt tried
--   and the file path for the failed file will be displayed.
validFastaFiles :: TestTree
validFastaFiles = testGroup "fastaStreamParser" [withResource validContents release validateFiles]
  where 
    parse'  (path,content) = parse fastaStreamParser path content
    success (path,content) = assertBool path . isRight $ parse' (path,content)
    release = const $ pure ()
    validContents = getFileContentsInDirectory "test/data-sets/fasta/valid"
    validateFiles :: IO (Map FilePath String) -> TestTree
    validateFiles !filesIO = testGroup "Valid files" [testCase "Unexpected parse errors" fileTree]
      where 
        fileTree = do
          files <- toList <$> filesIO
          sequence_ $ success <$> files
{-
validFastaFiles' :: TestTree
validFastaFiles' = testGroup "fastaStreamParser" [withResource validContents release validateFiles]
  where 
    parse'  (path,content) = parse fastaStreamParser path content
    success (path,content) = testCase (show path) . assert . isRight $ parse' (path,content)
    release = const $ pure ()
    validContents = getFileContentsInDirectory "test/data-sets/fasta/valid"
    validateFiles :: IO (Map FilePath String) -> TestTree
    validateFiles !filesIO = testGroup "Valid files" $ unsafePerformIO fileTree
      where 
        fileTree = fmap success . toList <$> filesIO
-}

-- | Gets all the given files and thier contents in the specified directory
getFileContentsInDirectory :: FilePath -> IO (Map FilePath String)
getFileContentsInDirectory path = do
    files <- filter isFile <$> getDirectoryContents path
    sequence . fromList $ (id &&& readFile) . withPath <$> files
  where
    isFile = not . all (=='.')
    withPath file = if last path /= '/'
                    then concat [path,"/",file] 
                    else concat [path,    file] 
















isLeft, isRight :: Either a b  -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight = not . isLeft

headOrEmpty :: [[a]] -> [a]
headOrEmpty = maybe [] id . headMay
