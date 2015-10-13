{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module File.Format.Fasta.Test
  ( testSuite
  ) where

import Data.Char                  (isSpace)
import File.Format.Fasta.Internal
import File.Format.Fasta.Parser
import Safe                       (headMay)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestSuite.Internal
import Text.Parsec                (parse,eof)

testSuite :: TestTree
testSuite = testGroup "Fasta Format"
  [ testGroup "Fasta Generalized Combinators" [identifier',commentBody',identifierLine']
  , testGroup "Fasta Parser" [fastaSequence']
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

{-
isLeft, isRight :: Either a b  -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight = not . isLeft
-}

headOrEmpty :: [[a]] -> [a]
headOrEmpty = maybe [] id . headMay
