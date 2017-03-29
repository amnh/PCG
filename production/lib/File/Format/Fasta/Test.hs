{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fasta.Test
  ( testSuite
  , validTaxonLines
  ) where


import Control.Arrow              (second)
import Data.Char                  (isSpace)
import Data.Maybe                 (fromMaybe)
import File.Format.Fasta.Internal
import File.Format.Fasta.Parser
import Safe                       (headMay)
import Test.Custom.Parse          (parseEquals,parseFailure,parserSatisfies)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec            (parse,eof)


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
    success str   = testCase (show str) $ parseEquals  (identifier <* eof) str str
    failure str   = testCase (show str) $ parseFailure (identifier <* eof) str
    invalidTaxonLabels =
      [ x ++ [s] ++ y |  e    <- validTaxonLabels
                      ,  s    <- "$ \t\r\n"
                      ,  i    <- [length e `div` 2]
                      , (x,y) <- [i `splitAt` e] 
      ]
    invariant = testProperty "fastaLabel invariant" f
      where 
--        f x = null str || parseEquals identifier str == res
        f x = null str || parserSatisfies identifier str (== res)
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
           || parserSatisfies commentBody x (== res)
          where
            res = unwords . words $ takeWhile (not.(`elem`"\n\r")) x
            hasLeadingDollarSign = let y = dropWhile isSpace x
                                   in  headMay y == Just '$'
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
    validComments = testGroup "Valid comments" $ success <$> validCommentBodies
      where 
        success str = testCase (show str) $ parseEquals (commentBody <* eof) str "A species of animal"


validCommentBodies :: [String]
validCommentBodies =
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


validTaxonCommentLines     :: [(String, String)]
validTaxonCommentLines     = zip validTaxonLabels validTaxonCommentedLabels 
validTaxonCommentlessLines :: [(String, String)]
validTaxonCommentlessLines = zip  validTaxonLabels (inlineLabel <$> validTaxonLabels)
validTaxonCommentedLabels  :: [String]
validTaxonCommentedLabels  = inlineLabel <$> zipWith (<>) validTaxonLabels validCommentBodies
inlineLabel :: String -> String
inlineLabel x = concat ["> ", x, "\n"]


fastaSequence' :: TestTree
fastaSequence' = testGroup "fastaSequence" [valid,nonDNAValid]
  where
    valid             = testGroup "Valid DNA sequences"     $ success <$> validSequences
    nonDNAValid       = testGroup "Valid non-DNA sequences" $ success <$> nonDNASequences
    success (res,str) = testCase (show str) $ parseEquals fastaSequence str res
    nonDNASequences   = [ ("-.?"                 , "-.?\n"                 ) -- Gap / Missing
                        , ("#"                   , "#\n"                   ) -- Sequence Partition 
                        , ("RYSWKMBDHVN"         , "RYSWKMBDHVN\n"         ) -- IUPAC Ambiguity Codes
                        , ("ACDEFGHIKLMNPQRSTVWY", "ACDEFGHIKLMNPQRSTVWY\n") -- AminoAcids
                        ]


-- add X as ambiguity for AminoAcids
validSequences :: [(String,String)]
validSequences =
    [ ("-GATACA-"            , "-GATACA-\n"            )
    , ("-GATACA-"            , "- G ATA CA- \n"        )
    , ("-GATACA-"            , "-GAT\nACA-\n"          )
    , ("-GATACA-"            , " -G A\nT\n AC A- \n"   )
    , ("-GATACA-"            , "-GA\n\nT\n \nACA-\n"   )
    ]


fastaTaxonSequenceDefinition' :: TestTree
fastaTaxonSequenceDefinition' = testGroup "fastaTaxonSequenceDefinition" [valid]
  where
    valid             = testGroup "Valid sequences" $ success <$> validTaxonSequences
    success (res,str) = testCase (show str) $ parseEquals fastaTaxonSequenceDefinition str res


validTaxonLines     :: [(String,String)]
validTaxonLines     = validTaxonCommentLines ++ validTaxonCommentlessLines
validTaxonSequences :: [(FastaSequence,String)]
validTaxonSequences = zipWith f validTaxonLines validSequences
  where
    f (x,str) (y,seq')  = (FastaSequence x y, concat [str,"\n",seq'])


fastaStreamParser' :: TestTree
fastaStreamParser' = testGroup "fastaStreamParser" [testGroup "Valid stream" [validStream]]
  where
    validStream = testCase "Concatenateed fasta stream" $ parseEquals fastaStreamParser str res
    (res,str)   = second concat $ unzip validTaxonSequences


headOrEmpty :: [[a]] -> [a]
headOrEmpty = fromMaybe [] . headMay

