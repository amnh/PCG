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
      [unquotedLabel',quotedLabel',newickBranchLength',newickLeaf',descendantList']
  , testGroup "Newick Parser" 
      [newickStandardDefinition']
  , testGroup "Extended Newick Parser"
      [newickExtendedDefinition']
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

newickStandardDefinition' :: TestTree
newickStandardDefinition' = testGroup "newickStandardDefinition" [valid,invalid]
  where
    valid         = testGroup "Valid Newick trees"   $ success <$> validTrees
    invalid       = testGroup "Invalid Newick trees" $ failure <$> invalidTrees
    success str   = testCase (show str) $ parseSuccess (newickStandardDefinition <* eof) str
    failure str   = testCase (show str) $ parseFailure (newickStandardDefinition <* eof) str
    validTrees =
      [ "(left,right)root:1;"
      , "(((1,2),3),(4,5));"
      ]
    invalidTrees =
      [ "(left,right)root:1"  -- Missing ending semicolon
      , "(((1,2),3),(4,5):);" -- Missing length after colon
      ]

newickExtendedDefinition' :: TestTree
newickExtendedDefinition' = testGroup "newickExtendedDefinition" [valid,invalid]
  where
    valid         = testGroup "Valid Newick trees"   $ success <$> validTrees
    invalid       = testGroup "Invalid Newick trees" $ failure <$> invalidTrees
    success str   = testCase (show str) $ parseSuccess (newickExtendedDefinition <* eof) str
    failure str   = testCase (show str) $ parseFailure (newickExtendedDefinition <* eof) str
    validTrees =
      [ "(((1,2),X),((3,4)X,5));" -- Acyclical node merge
      ]
    invalidTrees =
      [ "(((1,2),X)Y,((3,Y)X,4));" -- Cyclic node merge, non-sensical
      ]
