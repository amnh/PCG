{-# LANGUAGE FlexibleContexts #-}

module File.Format.Nexus.Test
  ( testSuite
  ) where

import           Control.Monad              (join)
import           Data.Char
import           Data.DList                 (DList)
import qualified Data.DList as DL           (empty,fromList)
import           Data.Either.Combinators    (isLeft,isRight)
import qualified Data.Map as M
import           Data.Set                   (toList)
import           File.Format.Nexus.Data
import           File.Format.Nexus.Parser
import           File.Format.Nexus.Validate
import           Test.Custom                (parseEquals,parseFailure,parseSuccess)
import           Test.Tasty                 (TestTree,testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Megaparsec            (char,eof,parse,string)

import Debug.Trace (trace)

testSuite :: TestTree
testSuite = testGroup "Nexus Format"
  [ testGroup "Nexus Combinators" [ assumptionFieldDef'
                                  , blockend'
                                  , booleanDefinition'
                                  , charFormatFieldDef'
                                  , deInterleave'
                                  , getTaxonAndSeqFromMatrixRow'
                                  , formatDefinition'
                                  , ignoredSubBlockDef'
                                  , notKeywordWord'
                                  , quotedStringDefinition'
                                  , replaceMatches'
                                  , stringDefinition'
                                  , stringListDefinition'
                                  , tcmMatrixDefinition'
                                  , treeDefinition'
                                  ]
  ]

assumptionFieldDef' :: TestTree
assumptionFieldDef' = testGroup "assumptionFieldDef" [test1, test2]
    where
        test1 = testCase "Valid TCMMat" $ parseSuccess assumptionFieldDef validtcmMatrix
        test2 = testCase "Valid IgnAF" $ parseSuccess assumptionFieldDef "other;"

--TODO: break out failure cases for consumption of input
blockend' :: TestTree
blockend' = testGroup "blockend: Input should never be consumed" [end, endWithSemi, endblock, endblockWithSemi, other]
    where
        end              = testCase "END should fail"       $ parseFailure (blockend <* string "end" )     "end"
        endblock         = testCase "ENDBLOCK should fail"  $ parseFailure (blockend <* string "endblock") "endblock"
        endWithSemi      = testCase "END; should pass"      $ parseSuccess blockend "end;"
        endblockWithSemi = testCase "ENDBLOCK; should pass" $ parseSuccess blockend "endblock;"
        -- TODO: make this /actually/ arbitrary
        other = testCase "arbitrary other text" $ parseFailure blockend "other;"

booleanDefinition' :: TestTree
booleanDefinition' = testGroup "booleanDefinition" [generalProperty]
  where
    generalProperty :: TestTree
    generalProperty = testProperty "General boolean flag structure" f
      where
        f x = null x
           || parse (booleanDefinition x <* eof) "" x == Right True

charFormatFieldDef' :: TestTree
charFormatFieldDef' = testGroup "charFormatFieldDef" ([emptyString] ++ testSingletons ++ testCommutivity)
    where
        emptyString = testCase "Empty String" $ parseEquals charFormatFieldDef "" []
        testSingletons = map (\(x,y) -> testCase x (parseEquals charFormatFieldDef x [y])) stringTypeList
        testCommutivity = map (\(x,y) -> testCase x (parseEquals charFormatFieldDef x y)) stringTypeListPerms
        stringTypeList = [ ("datatype=xyz", CharDT "xyz")
                         , ("symbols=\"abc\"", SymStr (Right ["abc"]))
                         , ("transpose", Transpose True)
                         , ("interleave", Interleave True)
                         , ("tokens", Tokens True)
                         , ("equate=\"a={bc} d={ef}\"", EqStr (Right ["a={bc}", "d={ef}"]))
                         , ("missing=def", MissStr "def")
                         , ("gap=-", GapChar "-")
                         , ("matchchar=.", MatchChar ".")
                         , ("items=ghi", Items "ghi")
                         , ("respectcase", RespectCase True)
                         , ("nolabels", Unlabeled True)
                         , ("something ", IgnFF "something")
                         ]
        stringTypeListPerms = [(string ++ " " ++ string', [result, result']) | (string, result) <- stringTypeList, (string', result') <- stringTypeList]

deInterleave' :: TestTree
deInterleave' = testCase "deInterleave" $ assertEqual "" outMap (deInterleave inMap inTups)
    where
        inMap = M.fromList [("a",""),("e",""),("i","")]
        inTups = [("a","123"),("e","fgh"),("i","789")
                 ,("a","456"),("e","ijk"),("i","456")
                 ,("a","789"),("e","lmn"),("i","123")]
        outMap = M.fromList [("a", "123456789"),("e","fghijklmn"),("i","789456123")]

formatDefinition' :: TestTree
formatDefinition' = testGroup "formatDefinition" [test1, test2, test3, test4, test5, test6]
    where
        test1 = testCase "transpose" $ parseEquals formatDefinition "format transpose;" $ CharacterFormat "" (Right [""]) (Right [""]) "" "" "" "" False False True False False
        test2 = testCase "datatype" $ parseEquals formatDefinition "format DATATYPE=Standard;" $ CharacterFormat "Standard" (Right [""]) (Right [""]) "" "" "" "" False False False False False
        test3 = testCase "symbols" $ parseEquals formatDefinition "format symbols=\"abcd\";" $ CharacterFormat "" (Right ["abcd"]) (Right [""]) "" "" "" "" False False False False False
        test4 = testCase "missing" $ parseEquals formatDefinition "format MISSING=?;" $ CharacterFormat "" (Right [""]) (Right [""]) "?" "" "" "" False False False False False
        test5 = testCase "gap" $ parseEquals formatDefinition "format GAP= -;" $ CharacterFormat "" (Right [""]) (Right [""]) "" "-" "" "" False False False False False
        test6 = testCase "all previous 5" $ parseEquals formatDefinition "FORMAT DATATYPE=Standard symbols=\"abcd\" MISSING=? GAP= - transpose;" $ CharacterFormat "Standard" (Right ["abcd"]) (Right [""]) "?" "-" "" "" False False True False False

getTaxonAndSeqFromMatrixRow' :: TestTree
getTaxonAndSeqFromMatrixRow' = testGroup "getTaxonAndSeqFromMatrixRow" [space,spaces,tab,tabs,both]
    where
        space = testProperty "One space" f
            where
                f :: NonEmptyList AsciiAlphaNum -> AsciiAlphaNum -> NonEmptyList Char -> Bool
                f x prefix y = getTaxonAndSeqFromMatrixRow combo == (tax,seq)
                    where
                        prefix' = getAsciiAlphaNum prefix
                        tax = getAsciiAlphaNum <$> getNonEmpty x
                        seq = prefix' : getNonEmpty y
                        combo = tax ++ " " ++ seq
        spaces = testProperty "Multiple spaces" f
            where
                f :: NonEmptyList AsciiAlphaNum -> AsciiAlphaNum -> NonEmptyList Char -> Bool
                f x prefix y = getTaxonAndSeqFromMatrixRow combo == (tax,seq)
                    where
                        prefix' = getAsciiAlphaNum prefix
                        tax = getAsciiAlphaNum <$> getNonEmpty x
                        seq = prefix' : getNonEmpty y
                        combo = tax ++ "     " ++ seq
        tab = testProperty "Single tab" f
            where
                f :: NonEmptyList AsciiAlphaNum -> AsciiAlphaNum -> NonEmptyList Char -> Bool
                f x prefix y = getTaxonAndSeqFromMatrixRow combo == (tax,seq)
                    where
                        prefix' = getAsciiAlphaNum prefix
                        tax = getAsciiAlphaNum <$> getNonEmpty x
                        seq = prefix' : getNonEmpty y
                        combo = tax ++ "\t" ++ seq
        tabs = testProperty "Multiple tabs" f
            where
                f :: NonEmptyList AsciiAlphaNum -> AsciiAlphaNum -> NonEmptyList Char -> Bool
                f x prefix y = getTaxonAndSeqFromMatrixRow combo == (tax,seq)
                    where
                        prefix' = getAsciiAlphaNum prefix
                        tax = getAsciiAlphaNum <$> getNonEmpty x
                        seq = prefix' : getNonEmpty y
                        combo = tax ++ "\t\t\t\t" ++ seq
        both = testProperty "Combination of tabs & spaces" f
            where
                f :: NonEmptyList AsciiAlphaNum -> NonEmptyList InlineSpace -> AsciiAlphaNum -> NonEmptyList Char -> Bool
                f x y prefix z = getTaxonAndSeqFromMatrixRow combo == (tax,seq)
                    where
                        prefix' = getAsciiAlphaNum prefix
                        tax = getAsciiAlphaNum <$> getNonEmpty x
                        sep = getInlineSpaceChar <$> getNonEmpty y
                        seq = prefix' : getNonEmpty z
                        combo = tax ++ sep ++ seq

ignoredSubBlockDef' :: TestTree
ignoredSubBlockDef' = testGroup "ignoredSubBlockDef" [endTest, endblockTest, sendTest, semicolonTest, argumentTest, emptyStringTest]
    where
--        justDelimiter = tesCase
        endTest = testProperty "END;" f
            where
                f :: Bool
                f = isLeft $ parse (ignoredSubBlockDef ';' <* eof) "" "end;"
        endblockTest = testProperty "ENDBLOCK;" f
            where
                f :: Bool
                f = isLeft $ parse (ignoredSubBlockDef ';' <* eof) "" "endblock;"
        sendTest = testProperty "Some word that ends with \"end;\"" f
            where
                f :: NonEmptyList AsciiAlphaNum -> Bool
                f x = parse (ignoredSubBlockDef ';' <* eof) "" inp == Right res
                    where
                        x'  = getAsciiAlphaNum <$> getNonEmpty x
                        res = x' ++ "end"
                        inp = res ++ ";"
        semicolonTest = testProperty "Block ends with \";\"" f
            where
                f :: NonEmptyList AsciiAlphaNum -> Bool
                f x = parse (ignoredSubBlockDef ';' <* eof) "" inp == Right x'
                    where
                        x'  = getAsciiAlphaNum <$> getNonEmpty x
                        inp = x' ++ ";"
        argumentTest = testProperty "Block ends with a designated delimiter" f
            where
                f :: (NonEmptyList AsciiAlphaNum, AsciiNonAlphaNum) -> Bool
                f (x,y) = parse (ignoredSubBlockDef arg <* eof) "" inp == Right x'
                    where
                        arg = getAsciiNonAlphaNum y
                        x'  = getAsciiAlphaNum <$> getNonEmpty x
                        inp = x' ++ [arg]
        emptyStringTest = testCase "Empty String" $ parseFailure (ignoredSubBlockDef ' ' <* eof) ";"

notKeywordWord' :: TestTree
notKeywordWord' = testGroup "notKeywordWord" [rejectsKeywords, semicolonTest, withSpace, argumentTest]
  where
    rejectsKeywords = testProperty "Rejects keywords" f
      where
        -- f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f x = isLeft $ parse (notKeywordWord "" <* eof) "" str
          where
            str = getNexusKeyword x
    semicolonTest = testProperty "Block ends with \";\", no argument" f
      where
        f :: NonEmptyList AsciiAlphaNum -> Bool
        f x = parse (notKeywordWord "" <* char ';' <* eof) "" inp == Right x'
            where
                x'  = getAsciiAlphaNum <$> getNonEmpty x
                inp = x' ++ ";"
    withSpace = testProperty "Input has spaces; return string to first space, no argument" f
      where
        f :: (NonEmptyList AsciiAlphaNum, Char) -> Bool
        f (x,y) = parse (notKeywordWord "" <* char ' ' <* char y <* eof) "" str == Right x'
          where
            x'  = getAsciiAlphaNum <$> getNonEmpty x
            str = x' ++ " " ++ [y]
    argumentTest = testProperty "Input ends with a designated delimiter" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList Char) -> Bool
        f (x,y) = and $ fmap (\(anArg,inp) -> parse (notKeywordWord args <* char anArg <* eof) "" inp == Right x') inp'
          where
            x'   = getAsciiAlphaNum <$> getNonEmpty x
            args = filter (`notElem` x') $ getNonEmpty y
            inp' = [(n,m++[n]) | n <- args, m <- [x']]

quotedStringDefinition' :: TestTree
quotedStringDefinition' = testGroup "quotedStringDefinition" [generalProperty, missingCloseQuote, rejectsKeywords, withSpace]
  where
    badChars = "[;\""
    generalProperty = testProperty "General quoted string definition: key=\"space delimited values\", capture values" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList Char) -> Bool
        f (x,y) = null res || parse (quotedStringDefinition key <* eof) "" str == Right (Right res)
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = filter (`notElem` badChars) $ getNonEmpty y
            res = words val
            str = key ++ "=\"" ++ val ++ "\""

    missingCloseQuote = testProperty "Missing close quote" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList AsciiAlphaNum) -> Bool
        f (x,y) = (isLeft <$> parse (quotedStringDefinition key <* eof) "" str) == Right True
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getAsciiAlphaNum <$> getNonEmpty y
            str = key ++ "=\"" ++ val
    rejectsKeywords = testProperty "Rejects keywords" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f (x,y) = isLeft $ parse (quotedStringDefinition key <* eof) "" str
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getNexusKeyword y
            str = key ++ "=\"" ++ val ++ "\""
    withSpace = testProperty "With 1 or more space characters after the =" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList Char, NonEmptyList Whitespace) -> Bool
        f (x,y,z) = null res || parse (quotedStringDefinition key <* eof) "" str == Right (Right res)
          where
            key = getAsciiAlphaNum  <$> getNonEmpty x
            val = filter (`notElem` badChars) $ getNonEmpty y
            res = words val
            spc = getWhitespaceChar <$> getNonEmpty z
            str = key ++ "=" ++ spc ++ "\"" ++ val ++ "\""

replaceMatches' :: TestTree
replaceMatches' = testCase "replaceMatches" $ assertEqual "" outString (replaceMatches matchChar canonical strToRepair)
    where
        matchChar   = '.'
        canonical   = ".b.d.f.h.j.l.n."
        strToRepair = "a.c.e.g.i.k.m.o"
        outString   = "abcdefghijklmno"

stringDefinition' :: TestTree
stringDefinition' = testGroup "stringDefinition" [generalProperty, withSpace, rejectsKeywords]
  where
    generalProperty = testProperty "General string definition: key=value, capture value" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList AsciiAlphaNum) -> Bool
        f (x,y) = parse (stringDefinition key <* eof) "" str == Right val
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getAsciiAlphaNum <$> getNonEmpty y
            str = key ++ "=" ++ val
    withSpace = testProperty "With 1 or more space characters after the =" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NonEmptyList AsciiAlphaNum, NonEmptyList Whitespace) -> Bool
        f (x,y,z) = parse (stringDefinition key <* eof) "" str == Right val
          where
            key = getAsciiAlphaNum  <$> getNonEmpty x
            val = getAsciiAlphaNum  <$> getNonEmpty y
            spc = getWhitespaceChar <$> getNonEmpty z
            str = key ++ "=" ++ spc ++ val
    rejectsKeywords = testProperty "Rejects Keywords" f
      where
        f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
        f (x,y) = isLeft $ parse (stringDefinition key <* eof) "" str
          where
            key = getAsciiAlphaNum <$> getNonEmpty x
            val = getNexusKeyword y
            str = key ++ "=" ++ val

stringListDefinition' :: TestTree
stringListDefinition' = testGroup "stringListDefinition" [test1, test2, rejectsKeywords]
    where
        test1 = testCase "Nothing between label and ;" $ parseEquals (stringListDefinition "label1") "label1;" []
        test2 = testCase "Label doesn't match" $ parseFailure (stringListDefinition "label2") "label1 abc;"
        rejectsKeywords = testProperty "Rejects Keywords" f
          where
            f :: (NonEmptyList AsciiAlphaNum, NexusKeyword) -> Bool
            f (x,y) = isLeft $ parse (stringListDefinition key <* eof) "" str
              where
                key = getAsciiAlphaNum <$> getNonEmpty x
                val = getNexusKeyword y
                str = key ++ " " ++ val ++ ";"

tcmMatrixDefinition' :: TestTree
tcmMatrixDefinition' = testGroup "tcmMatrixDefinition" [test1]
    where
        test1 = testCase "Success given a perfectly formatted block" $ parseSuccess tcmMatrixDefinition validtcmMatrix


treeDefinition' :: TestTree
treeDefinition' = testGroup "treeDefinition" [failsOnEmptyTree, succeedsOnSimple, withBranchLengths, withComments, arbitraryName, rootedAnnotation, unrootedAnnotation]
  where
    failsOnEmptyTree   = testCase "Fails on 'empty tree'"                $ parseFailure treeDefinition "TREE emptyTree  = ;"
    succeedsOnSimple   = testCase "Parses simple tree"                   $ parseSuccess treeDefinition "TREE simpleTree = (A,B);"
    withBranchLengths  = testCase "Parses tree with branch lengths"      $ parseSuccess treeDefinition "TREE branchNums = (A:42,B:1.337):13;"
    withComments       = testCase "Parses tree with internal comments"   $ parseSuccess treeDefinition "TREE comments   = (A[left], B[right]);"
    rootedAnnotation   = testCase "Parses tree with rooted annotation"   $ parseSuccess treeDefinition "TREE rooted   = [&R] (A,B);"
    unrootedAnnotation = testCase "Parses tree with unrooted annotation" $ parseSuccess treeDefinition "TREE unrooted = [&U] (A,B);"
    arbitraryName      = testProperty "Arbitrary labeled tree" f
      where
        f :: NonEmptyList AsciiAlphaNum -> Bool
        f randomLabel = case parse (treeDefinition <* eof) "" (unwords ["Tree ", treeLabel, "=", "(A,B);"]) of
                          Right (treeLabel', _) -> treeLabel == treeLabel'
                          Left  _               -> False
          where
            treeLabel = getAsciiAlphaNum <$> getNonEmpty randomLabel

stringTypeList =
                 [ ("datatype=xyz", CharDT "xyz")
                 , ("symbols=\"abc\"", SymStr (Right ["abc"]))
                 , ("transpose", Transpose True)
                 , ("interleave", Interleave True)
                 , ("tokens", Tokens True)
                 , ("equate=\"a={bc} d={ef}\"", EqStr (Right ["a={bc}", "d={ef}"]))
                 , ("missing=def", MissStr "def")
                 , ("gap=-", GapChar "-")
                 , ("matchchar=.", MatchChar ".")
                 , ("items=ghi", Items "ghi")
                 , ("respectcase", RespectCase True)
                 , ("nolabels", Unlabeled True)
                 , ("something ", IgnFF "something")
                 ]

stringTypeListPerms = [(string ++ " " ++ string', [result, result']) | (string, result) <- stringTypeList, (string', result') <- stringTypeList]

validtcmMatrix = "usertype name  = 4\n [a]A B C D\n 0 1 2 3\n 1 0 2 3\n 1 2 0 3\n 1 2 3 0\n;"


newtype AsciiAlphaNum    = AsciiAlphaNum    { getAsciiAlphaNum    :: Char } deriving (Eq)
newtype AsciiNonAlphaNum = AsciiNonAlphaNum { getAsciiNonAlphaNum :: Char } deriving (Eq)
newtype Whitespace       = Whitespace       { getWhitespaceChar   :: Char } deriving (Eq)
newtype InlineSpace      = InlineSpace      { getInlineSpaceChar  :: Char } deriving (Eq)

nonSpaceChars    = fmap AsciiAlphaNum    . filter        isAlphaNum  $ chr <$> [0..128]
nonAlphaNumChars = fmap AsciiNonAlphaNum . filter (not . isAlphaNum) $ chr <$> [0..128]
whitespaceChars  = fmap Whitespace " \t\n\r\f\v"
inlineSpaceChars = fmap InlineSpace " \t"

instance Arbitrary AsciiAlphaNum where
  arbitrary = elements nonSpaceChars

instance Show AsciiAlphaNum where
  show (AsciiAlphaNum c) = show c

instance Arbitrary AsciiNonAlphaNum where
  arbitrary = elements nonAlphaNumChars

instance Show AsciiNonAlphaNum where
  show (AsciiNonAlphaNum c) = show c

instance Arbitrary Whitespace where
  arbitrary = elements whitespaceChars

instance Show Whitespace where
  show (Whitespace c) = show c

instance Arbitrary InlineSpace where
  arbitrary = elements inlineSpaceChars

instance Show InlineSpace where
  show (InlineSpace c) = show c

newtype NexusKeyword = NexusKeyword String deriving (Eq)

instance Arbitrary NexusKeyword where
  arbitrary = elements . fmap NexusKeyword $ toList nexusKeywords

instance Show NexusKeyword where
  show (NexusKeyword c) = show c

getNexusKeyword (NexusKeyword c) = c
