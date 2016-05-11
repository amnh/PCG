{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick.Test
  ( testSuite
  ) where

import Data.Either.Combinators    (isRight,rightToMaybe)
import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Test.Custom.Parse          (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                 (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec            (eof,parse)

testSuite :: TestTree
testSuite = testGroup "Newick Format"
  [ testGroup "Newick Combinators"
      [unquotedLabel',quotedLabel',newickBranchLength',newickLeaf',descendantList']
  , testGroup "Newick Parser" 
      [newickStandardDefinition']
  , testGroup "Extended Newick Parser"
      [newickExtendedDefinition']
  , testGroup "Forest Newick Parser"
      [newickForestDefinition']
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
        result     = Right $ NewickNode [] (rightToMaybe labelValue) (Just num) 
    
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
    valid         = testGroup "Valid Newick trees"   $ success <$> validStandardTrees
    invalid       = testGroup "Invalid Newick trees" $ failure <$> invalidStandardTrees
    success str   = testCase (show str) $ parseSuccess (newickStandardDefinition <* eof) str
    failure str   = testCase (show str) $ parseFailure (newickStandardDefinition <* eof) str
    invalidStandardTrees =
      [ "(left,right)root:1"  -- Missing ending semicolon
      , "(((1,2),3),(4,5):);" -- Missing length after colon
      ]

validStandardTrees :: [String]
validStandardTrees =
      [ "(left,right)root:1;"
      , "(((1,2),3),(4,5));"
      ]

newickExtendedDefinition' :: TestTree
newickExtendedDefinition' = testGroup "newickExtendedDefinition" [valid,invalid]
  where
    valid         = testGroup "Valid Newick trees"   $ success <$> validExtendedTrees
    invalid       = testGroup "Invalid Newick trees" $ failure <$> invalidExtendedTrees
    success str   = testCase (show str) $ parseSuccess (newickExtendedDefinition <* eof) str
    failure str   = testCase (show str) $ parseFailure (newickExtendedDefinition <* eof) str
    invalidExtendedTrees =
      [ "(((1,2),X)Y,((3,Y)X,4));" -- Cyclic node merge, non-sensical
      ]

validExtendedTrees :: [String]
validExtendedTrees =
      [ "(((1,2),X),((3,4)X,5));" -- Acyclical node merge
      ]

newickForestDefinition' :: TestTree
newickForestDefinition' = testGroup "newickForestDefinition" [valid,invalid]
  where
    valid          = testGroup "Valid Newick trees"   $ success <$> validForests
    invalid        = testGroup "Invalid Newick trees" $ failure <$> invalidForests
    success str    = testCase (show str) $ parseSuccess (newickForestDefinition <* eof) str
    failure str    = testCase (show str) $ parseFailure (newickForestDefinition <* eof) str
    validForests   = [concat ["<", concat validStandardTrees, concat validExtendedTrees, ">"]]
    invalidForests =
      [ "(((1,2),X),((3,4)X,5));" -- no angle braces
      ]

    
