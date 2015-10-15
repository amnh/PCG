{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick.Test
  ( testSuite
  ) where

import Data.Either.Custom         (isRight,rightMay)
import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Test.Custom                (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec                (parse,eof)

testSuite :: TestTree
testSuite = testGroup "Newick Format"
  [ testGroup "Newick Combinators"
      [unquotedLabel',quotedLabel',newickBranchLength']
  , testGroup "Newick Parser" 
      [newickLeaf',descendantList']
  , testGroup "Extended Newick Parser"
      []
  , testGroup "Forest Newick Parser"
      []
  , testGroup "Newick Converter"
      []
  ]


validUnquotedLabels :: [String]
validUnquotedLabels =
      [ "Peripatidae"
      , "Colossendeis"
      , "Ammotheidae"
      , "Buthidae"
      , "Mygalomorphae"
      , "Homo_Sapien"
      ]

invalidUnquotedLabels :: [String]
invalidUnquotedLabels =
      [ r |  e    <- validUnquotedLabels
          ,  c    <- requiresQuotedLabelChars
          ,  i    <- [length e `div` 2]
          , (x,y) <- [i `splitAt` e]
          ,  r    <- [x++[c]++y]
      ]

unquotedLabel' :: TestTree
unquotedLabel' = testGroup "unquotedLabel" [validLabels,invalidLabels]
  where
    validLabels   = testGroup "Valid unquoted labels"   $ success <$> validUnquotedLabels
    invalidLabels = testGroup "Invalid unquoted labels" $ failure <$> invalidUnquotedLabels
    success str   = testCase (show str) $ parseEquals   (unquotedLabel <* eof) str str
    failure str   = testCase (show str) $ parseFailure  (unquotedLabel <* eof) str

quotedLabel' :: TestTree
quotedLabel' = testGroup "quotedLabel" [validSpecialChars,validEscaping,validEndingEscaping,enquotedInvariant]
  where
    validSpecialChars   = testGroup "Valid enquoted strings with special chars"         $ success <$> validSpecialLabels
    validEscaping       = testGroup "Valid enquoted strings with escaping"              $ success <$> validEscapedLabels
    validEndingEscaping = testGroup "Valid enquoted string with escaped last character" [ success ("prime'","'prime'''") ]
    success (res,str)   = testCase (show str) $ parseEquals  (quotedLabel <* eof) str res
    validSpecialLabels  =
      [ (r,s) | r <- filter ('\''`notElem`) invalidUnquotedLabels
              , s <- ["'"++r++"'"]
      ]
    validEscapedLabels =
      [ (r,s) |  e    <- validUnquotedLabels
              ,  i    <- [length e `div` 2]
              , (x,y) <- [i `splitAt` e]
              ,  r    <- [x ++"'"++ y]
              ,  s    <- ["'"++x++"''"++y++"'"]
      ]
    enquotedInvariant :: TestTree
    enquotedInvariant = testProperty "Unquoted label ==> quoted label invariant" f 
      where
        f :: String -> Property
        f x = parsingSuccess unquotedLabel x ==> parsingSuccess quotedLabel $ "'"++x++"'"
        parsingSuccess e = isRight . parse (e <* eof) ""

newickBranchLength' :: TestTree
newickBranchLength' = testGroup "newickBranchLengthDefinition" [invariant]
  where
    invariant = testProperty "Injective invariant" f
      where
        f x = Right x == parse (branchLengthDefinition <* eof) "" (':' : show x)

newickLeaf' :: TestTree
newickLeaf' = testGroup "newickLeafDefinition'" [invariant]
  where
    invariant = testProperty "Injective invariant" f
    f :: (String, Double) -> Property
    f (str,num) = validLabel ==> validLeaf
      where
        validLabel = isRight labelValue
        labelValue = parse (newickLabelDefinition <* eof) "" str
        validLeaf  = parse newickLeafDefinition "" target == result
        target     = str ++ ":" ++ show num
        result     = Right $ NewickNode [] (rightMay labelValue) (Just num) 
    
descendantList' :: TestTree
descendantList' = testGroup "descendantListDefinition" [valid,invalid]
  where
    valid         = testGroup "Valid subtree strings"   $ success <$> validSubtrees
    invalid       = testGroup "Invalid subtree strings" $ failure <$> invalidSubtrees
    success str   = testCase (show str) $ parseSuccess (descendantListDefinition <* eof) str
    failure str   = testCase (show str) $ parseFailure (descendantListDefinition <* eof) str
    validSubtrees =
      [ "(wow)"
      , "(wow,such:1337)"
      , "(wow,_such_:1337,'very''':42,(much:0.07))"
      ]
    invalidSubtrees =
      [ "()"                   -- Empty set
      , "((wow)"               -- Mismatched parens
      , "(wow such very much)" -- No commas
      ]

{-



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
    validWithoutComments = testGroup "Valid taxon label lines without comemnts" $ success <$> validTaxonCommentlessLines
    validWithComments    = testGroup "Valid taxon label lines with comments"    $ success <$> validTaxonCommentLines
    success (res,str)    = testCase (show str) $ parseEquals (identifierLine <* eof) str res

validTaxonCommentLines     :: [(String, String)]
validTaxonCommentLines     = zip validTaxonLabels validTaxonCommentedLabels 
validTaxonCommentlessLines :: [(String, String)]
validTaxonCommentlessLines = zip  validTaxonLabels (inlineLabel <$> validTaxonLabels)
validTaxonCommentedLabels  :: [String]
validTaxonCommentedLabels  = inlineLabel <$> zipWith (++) validTaxonLabels validCommentBodies
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

-}
