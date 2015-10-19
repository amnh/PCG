{-# LANGUAGE FlexibleContexts #-}

module File.Format.Newick.Test
  ( testSuite
  ) where

import Data.Either.Custom                      (isRight,rightMay)
import Data.List                               (intercalate)
import File.Format.TransitionCostMatrix.Parser
import Test.Custom                             (parseEquals,parseFailure,parseSuccess)
import Test.Tasty                              (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec                             (parse,eof)

testSuite :: TestTree
testSuite = testGroup "TCM Format"
  [ testGroup "TCM Combinators"
      [alphabetLine',matrixBlock']
  , testGroup "TCM Parser" 
      []
  , testGroup "TCM Converter"
      []
  ]


validAlphabets :: [String]
validAlphabets = appendNewlines
  [ "1 2 3"
  , "a b c"
  , "do re mi"
  , "\\alpha \\beta \\gamma"
  , "Wiskey Tango Foxtrot"
  , intercalate " " $ pure <$> ['a'..'z']
  , intercalate " " $ pure <$> ['0'..'9']
  ]

invalidAlphabets :: [String]
invalidAlphabets = appendNewlines
  [ ""            -- empty line
  , "a a"         -- duplicate entries
  , "a b c a b a" -- many duplicate entries
  ]

alphabetLine' :: TestTree
alphabetLine' = testGroup "alphabetLine" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid unquoted labels"   $ success <$> validAlphabets
    invalidLines = testGroup "Invalid unquoted labels" $ failure <$> invalidAlphabets
    success str  = testCase (show str) $ parseEquals   (alphabetLine <* eof) str (words str)
    failure str  = testCase (show str) $ parseFailure  (alphabetLine <* eof) str

matrixBlock' :: TestTree
matrixBlock' = testGroup "matrixBlock" [validBlocks,invalidBlocks]
  where
    validBlocks   = testGroup "Valid unquoted labels"   $ success <$> validMatricies
    invalidBlocks = testGroup "Invalid unquoted labels" $ failure <$> invalidMatricies
    success str   = testCase (show str) $ parseSuccess  (matrixBlock <* eof) str
    failure str   = testCase (show str) $ parseFailure  (matrixBlock <* eof) str

validMatricies :: [String]
validMatricies = appendNewlines
  [ "1 2 3\n4 5 6\n7 8 9"
  , "1.0 1.0\n1.0 0.0"
  ]

invalidMatricies :: [String]
invalidMatricies = appendNewlines
  [ ""              -- empty matrix
  , "1 2 3\n 4 5 6" -- not square matrix
  , "1 2 3\n4 5"    -- inconsistent column length
  ]



appendNewlines :: [String] -> [String]
appendNewlines = fmap (\x -> x ++"\n")

{--

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

--}
