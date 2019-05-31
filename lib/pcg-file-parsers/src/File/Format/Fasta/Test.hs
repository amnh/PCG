{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Fasta.Test
  ( testSuite
  , validTaxonLines
  ) where

import Control.Arrow              (first, second)
import Data.Char                  (isSpace)
import Data.Foldable
import Data.Maybe                 (fromMaybe)
import Data.String
import Data.Text.Short            (ShortText, toString)
import Data.Vector.Unboxed        (Vector, fromList)
import File.Format.Fasta.Internal
import File.Format.Fasta.Parser
import Test.Custom.Parse          (parseEquals, parseFailure, parserSatisfies)
import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec            (eof)


testSuite :: TestTree
testSuite = testGroup "Fasta Format"
    [ testGroup "Fasta Generalized Combinators"
        [identifier',commentBody',identifierLine']
    , testGroup "Fasta Parser"
        [fastaSequence',fastaTaxonSequenceDefinition',fastaStreamParser']
    , testGroup "Fasta Converter"
        []
    ]


identifier' :: TestTree
identifier' = testGroup "identifier" [invariant, valid, invalid]
  where
    valid         = testGroup "Valid taxon labels"   $ success <$>   validTaxonLabels
    invalid       = testGroup "Invalid taxon labels" $ failure <$> invalidTaxonLabels
    success txt   = testCase (show $ toString txt) $ parseEquals  (identifier <* eof) (toString txt) (toString txt)
    failure txt   = testCase (show $ toString txt) $ parseFailure (identifier <* eof) (toString txt)
    invalidTaxonLabels =
      [ fromString (x <> [s] <> y)
      |  e    <- toString <$> validTaxonLabels
      ,  s    <- "$ \t\r\n"
      ,  i    <- [length e `div` 2]
      , (x,y) <- [i `splitAt` e]
      ]
    invariant = testProperty "fastaLabel invariant" f
      where
        f x = null str || parserSatisfies identifier str (== res)
          where
            str = takeWhile validIdentifierChar x
            res = headOrEmpty $ words str


validTaxonLabels :: [Identifier]
validTaxonLabels = fromString <$>
    [ "Peripatidae"
    , "Colossendeis"
    , "Ammotheidae"
    , "Buthidae"
    , "Mygalomorphae"
    ]


commentBody' :: TestTree
commentBody' = testGroup "commentBody"
    [ generalComment
    , prependedDollarSign
    , validComments
    ]
  where
    generalComment :: TestTree
    generalComment = testProperty "General comment structure" f
      where
        f x = hasLeadingDollarSign
           || null res
           || parserSatisfies commentBody x (== res)
          where
            res = unwords . words $ takeWhile (not . (`elem`"\n\r")) x
            hasLeadingDollarSign =
                case dropWhile isSpace x of
                  []  -> False
                  x:_ -> x == '$'

    prependedDollarSign :: TestTree
    prependedDollarSign = testProperty "Comments defined with leading dollar sign" $ prepended '$'
      where
        prepended :: Char -> String -> Bool
        prepended c x = null x || null line'
                     || ( parserSatisfies commentBody (            c   : line) (==line')
                       && parserSatisfies commentBody (      ' ' : c   : line) (==line')
                       && parserSatisfies commentBody (      c   : ' ' : line) (==line')
                       && parserSatisfies commentBody (' ' : c   : ' ' : line) (==line')
                        )
          where
            line  = takeWhile (not . (`elem`"\n\r")) x
            line' = unwords $ words line

    validComments = testGroup "Valid comments" $ success . toString <$> validCommentBodies
      where
        success str = testCase (show str) $ parseEquals (commentBody <* eof) str "A species of animal"


validCommentBodies :: [ShortText]
validCommentBodies = fromString <$>
    [ "$A species of animal"
    , " $A species of animal"
    , "$ A species of animal"
    , " $ A species of animal"
    , " A species of animal"
    ]


identifierLine' :: TestTree
identifierLine' = testGroup "fastaLabelLine" [validWithoutComments, validWithComments]
  where
    validWithoutComments = testGroup "Valid taxon label lines without comments" $ success <$> validTaxonCommentlessLines
    validWithComments    = testGroup "Valid taxon label lines with comments"    $ success <$> validTaxonCommentLines
    success (res,str)    = testCase (show str) $ parseEquals (identifierLine <* eof) str res


validTaxonCommentLines     :: [(Identifier, String)]
validTaxonCommentLines     = zip validTaxonLabels validTaxonCommentedLabels
validTaxonCommentlessLines :: [(Identifier, String)]
validTaxonCommentlessLines = zip validTaxonLabels (inlineLabel <$> validTaxonLabels)
validTaxonCommentedLabels  :: [String]
validTaxonCommentedLabels  = inlineLabel <$> zipWith (<>) validTaxonLabels validCommentBodies
inlineLabel :: Identifier -> String
inlineLabel x = concat ["> ", toString x, "\n"]


fastaSequence' :: TestTree
fastaSequence' = testGroup "fastaSequence" [valid,nonDNAValid]
  where
    valid             = testGroup "Valid DNA sequences"     $ success <$> validSequences
    nonDNAValid       = testGroup "Valid non-DNA sequences" $ success <$> nonDNASequences
    success (res,str) = testCase (show str) $ parseEquals fastaSequence str res
    nonDNASequences   = fromPairs
        [ ("-.?"                 , "-.?\n"                 ) -- Gap / Missing
        , ("#"                   , "#\n"                   ) -- Sequence Partition
        , ("RYSWKMBDHVN"         , "RYSWKMBDHVN\n"         ) -- IUPAC Ambiguity Codes
        , ("ACDEFGHIKLMNPQRSTVWY", "ACDEFGHIKLMNPQRSTVWY\n") -- AminoAcids
        ]


-- add X as ambiguity for AminoAcids
validSequences :: [(Vector Char, String)]
validSequences = fromPairs
    [ ("-GATACA-", "-GATACA-\n"         )
    , ("-GATACA-", "- G ATA CA- \n"     )
    , ("-GATACA-", "-GAT\nACA-\n"       )
    , ("-GATACA-", " -G A\nT\n AC A- \n")
    , ("-GATACA-", "-GA\n\nT\n \nACA-\n")
    ]


fromPairs :: [(String, a)] -> [(Vector Char, a)]
fromPairs = fmap (first fromList)


fastaTaxonSequenceDefinition' :: TestTree
fastaTaxonSequenceDefinition' = testGroup "fastaTaxonSequenceDefinition" [valid]
  where
    valid              = testGroup "Valid sequences" $ success <$> validTaxonSequences
    success (res, str) = testCase (show str) $ parseEquals fastaTaxonSequenceDefinition str res


validTaxonLines     :: [(Identifier, String)]
validTaxonLines     = validTaxonCommentLines <> validTaxonCommentlessLines
validTaxonSequences :: [(FastaSequence, String)]
validTaxonSequences = zipWith f validTaxonLines validSequences
  where
    f (x, str) (y, seq') = (FastaSequence x y, fold [str, "\n", seq'])


fastaStreamParser' :: TestTree
fastaStreamParser' = testGroup "fastaStreamParser" [testGroup "Valid stream" [validStream]]
  where
    validStream = testCase "Concatenateed fasta stream" $ parseEquals fastaStreamParser str res
    (res, str)  = second fold $ unzip validTaxonSequences


headOrEmpty :: [[a]] -> [a]
headOrEmpty    [] = []
headOrEmpty (x:_) = x

