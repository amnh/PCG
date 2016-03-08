{-# LANGUAGE FlexibleContexts #-}

module File.Format.TNT.Test
  ( testSuite
  ) where

import           Control.Monad              (filterM,join)
import           Data.Char
import           Data.Either.Combinators    (isLeft,isRight)
import           Data.Foldable
import           Data.List                  (inits,nub)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty   as NE (fromList)
import qualified Data.Map as M
import           File.Format.TNT.Parser
import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.CNames
import           File.Format.TNT.Command.Procedure
import           File.Format.TNT.Command.TRead
import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           Test.Custom
import           Test.Tasty                 (TestTree,testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Megaparsec            (char,eof,parse,string)

testSuite :: TestTree
testSuite = testGroup "TNT Format"
  [ testGroup "TNT Combinators" [ internalCombinators
                                , testCommandCCode
                                , testCommandCNames
                                , testCommandProcedure
                                , testCommandTRead
                                , testCommandXRead
                                ] 
  ]

internalCombinators :: TestTree
internalCombinators = testGroup "General combinators used amongst TNT commands" [flexibleNonNegativeInt', flexiblePositiveInt', nonNegInt', keyword']
  where
    flexibleNonNegativeInt' = testGroup "Non-negative Int parsed flexibly" [parsesInts, parsesIntegralDoubles]
      where
        parsesInts = testProperty "Parses positive, signed Integer literals" f
          where
            f :: Int -> Bool
            f x = (x >= 0) == isRight (parse (flexibleNonNegativeInt "") "" $ show x)
        parsesIntegralDoubles = testProperty "Parses positive, signed integral valued Doubles" f
          where
            f :: Int -> Bool
            f x = (x >= 0) == isRight (parse (flexibleNonNegativeInt "") "" $ show (fromIntegral x :: Double))
    flexiblePositiveInt' = testGroup "Positive Int parsed flexibly" [parsesInts, parsesIntegralDoubles]
      where
        parsesInts = testProperty "Parses positive, signed Integer literals" f
          where
            f :: Int -> Bool
            f x = (x > 0) == isRight (parse (flexiblePositiveInt "") "" $ show x)
        parsesIntegralDoubles = testProperty "Parses positive, signed integral valued Doubles" f
          where
            f :: Int -> Bool
            f x = (x > 0) == isRight (parse (flexiblePositiveInt "") "" $ show (fromIntegral x :: Double))
    keyword' = testGroup "Kewords parsed flexibly" [prefixesMatching] 
      where
        prefixesMatching = testProperty "Parses all prefixes after requisite characters" f
          where
            f :: WordToken -> Positive Int -> Bool
            f tok pos = length str <= n || propertyLogic `all` targets
              where
                n               = getPositive pos
                str             = getWordToken tok
                propertyLogic x = matchesPadded x && failsOnDirty x
                matchesPadded   = isRight . parse' . (++" ")
                failsOnDirty    = isLeft  . parse' . (++"z")
                parse'          = parse (keyword str n) ""
                (req,rem)       = splitAt n str
                targets         = (req++) <$> inits rem
    nonNegInt'  = testGroup "Non-negative Int without flexibility" [parsesInts]
      where
        parsesInts = testProperty "Parses non-negative, unsigned Integer literals" f
          where
            f :: Int -> Bool
            f x = (x >= 0) == isRight (parse nonNegInt "" $ show x)

testCommandCCode :: TestTree
testCommandCCode = testGroup "CCODE command tests" [ccodeHeader',ccodeIndicies',ccodeAdditive',ccodeNonAdditive',ccodeActive',ccodeNonActive',ccodeSankoff',ccodeNonSankoff',ccodeWeight',ccodeStep',ccodeExamples]
  where
    indicies = [ "42", "3.", ".3", "2.5", "." ]
    ccodeHeader' = testGroup "CCODE command header options" headers
      where
        headers = header <$> (drop 2 $ inits "ccode")
        header str = testCase ("Parses component \"" ++ str ++ "\"") $ parseSuccess ccodeHeader (str ++ " ")
    ccodeIndicies' = testGroup "CCODE indexing format" indexFormats
      where
        indexFormats = zipWith makeFormat descriptions indicies
        makeFormat str idx = testCase ("Parse " ++ str ++ " format: \"" ++ idx ++ "\"") $ parseSuccess characterIndicies idx
        descriptions = [ "single index"
                       , "index to end of sequence"
                       , "start of sequence to index"
                       , "from index to index"
                       , "entire sequence"
                       ]
    ccodeAdditive' = testGroup "CCODE make character(s) additive" additiveExamples
      where
        additiveExamples = makeAdditive <$> targets
        makeAdditive str = testCase ("Additive example: \"" ++ str ++ "\"") $ parseSuccess ccodeAdditive str
        targets = [ "+" ++ pad ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeNonAdditive' = testGroup "CCODE make character(s) non-additive" nonAdditiveExamples
      where
        nonAdditiveExamples = makeNonAdditive <$> targets
        makeNonAdditive str = testCase ("Non-additive example: \"" ++ str ++ "\"") $ parseSuccess ccodeNonAdditive str
        targets = [ "-" ++ pad ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeActive' = testGroup "CCODE make character(s) active" activeExamples
      where
        activeExamples = makeActive <$> targets
        makeActive str = testCase ("active example: \"" ++ str ++ "\"") $ parseSuccess ccodeActive str
        targets = [ prefix ++ suffix
                  | prefix <- [ "[" ++ pad ++ unwords idxs
                              | idxs <- tail (powerSet indicies)
                              , pad  <- [""," "]]
                  , suffix <- ["","]"," ]"]
                  ]
    ccodeNonActive' = testGroup "CCODE make character(s) non-active" nonActiveExamples
      where
        nonActiveExamples = makeNonActive <$> targets
        makeNonActive str = testCase ("Non-active example: \"" ++ str ++ "\"") $ parseSuccess ccodeNonActive str
        targets = [ "]" ++ pad ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeSankoff' = testGroup "CCODE make character(s) sankoff" sankoffExamples
      where
        sankoffExamples = makeSankoff <$> targets
        makeSankoff str = testCase ("Sankoff example: \"" ++ str ++ "\"") $ parseSuccess ccodeSankoff str
        targets = [ prefix ++ suffix
                  | prefix <- [ "(" ++ pad ++ unwords idxs
                              | idxs <- tail (powerSet indicies)
                              , pad  <- [""," "]]
                  , suffix <- ["",")"," )"]
                  ]
    ccodeNonSankoff' = testGroup "CCODE make character(s) non-sankoff" nonSankoffExamples
      where
        nonSankoffExamples = makeNonSankoff <$> targets
        makeNonSankoff str = testCase ("Non-sankoff example: \"" ++ str ++ "\"") $ parseSuccess ccodeNonSankoff str
        targets = [ ")" ++ pad ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeWeight' = testGroup "CCODE set character(s) weight" weightExamples
      where
        weightExamples = makeWeight <$> targets
        makeWeight str = testCase ("Weight example: \"" ++ str ++ "\"") $ parseSuccess ccodeWeight str
        targets = [ "/" ++ pad ++ "1 " ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeStep' = testGroup "CCODE set character(s) step value" stepExamples
      where
        stepExamples = makeStep <$> targets
        makeStep str = testCase ("Step example: \"" ++ str ++ "\"") $ parseSuccess ccodeSteps str
        targets = [ "=" ++ pad ++ "1 " ++ unwords idxs
                  | idxs <- tail (powerSet indicies)
                  , pad  <- [""," "]
                  ]
    ccodeExamples = testGroup "Example CCODE commands" $ testExample <$> examples
      where
        testExample (desc,str) = testCase desc $ parseSuccess ccodeCommand str
        examples = zip [ "CCODE example command #"++ show x | x <- [1..]]
                   [ "cc (.) ;"
                   , "cc - 0.96;"
                   , "cc - 0.1317;"
                   , "cc + 82 98 101 104 106 139 147 155 174 179 215 244 245 251 262 265 269 286 289 313 323 346 359 383 386 398 399 435 448 453 479 484 496 497 516 545 546 551 574 591 625 638 639 646 648 661 662 669 687 758 768.773 782 821.825 894 896 897 903 906 1026 1081 1086 1094 1111 1149 1176 1179 1188 1203 1206 1207 1210 1213 1216 1219 1220 1240;"
                   , "cc - 0.38942;"
                   , "cc - 0.81;cc + 82;cc - 83.97;cc + 98;cc - 99.100;cc + 101;cc - 102.103;"
                   , "cc + 104;cc - 105;cc + 106;cc - 107.138;cc + 139;cc - 140.146;cc + 147;"
                   , "cc - 148.154;cc + 155;cc - 156.173;cc + 174;cc - 175.178;cc + 179;"
                   , "cc - 180.214;cc + 215;cc - 216.243;cc + 244.245;cc - 246.250;cc + 251;"
                   , "cc - 252.261;cc + 262;cc - 263.264;cc + 265;cc - 266.268;cc + 269;"
                   , "cc - 270.285;cc + 286;cc - 287.288;cc + 289;cc - 290.312;cc + 313;"
                   , "cc - 314.322;cc + 323;cc - 324.345;cc + 346;cc - 347.358;cc + 359;"
                   , "cc - 360.382;cc + 383;cc - 384.385;cc + 386;cc - 387.397;cc + 398.399;"
                   , "cc - 400.434;cc + 435;cc - 436.447;cc + 448;cc - 449.452;cc + 453;"
                   , "cc - 454.478;cc + 479;cc - 480.483;cc + 484;cc - 485.495;cc + 496.497;"
                   , "cc - 498.515;cc + 516;cc - 517.544;cc + 545.546;cc - 547.550;cc + 551;"
                   , "cc - 552.573;cc + 574;cc - 575.590;cc + 591;cc - 592.624;cc + 625;"
                   , "cc - 626.637;cc + 638.639;cc - 640.645;cc + 646;cc - 647;cc + 648;"
                   , "cc - 649.660;cc + 661.662;cc - 663.668;cc + 669;cc - 670.686;cc + 687;"
                   , "cc - 688.757;cc + 758;cc - 759.767;cc + 768.773;cc - 774.781;cc + 782;"
                   , "cc - 783.820;cc + 821.825;cc - 826.893;cc + 894;cc - 895;cc + 896.897;"
                   , "cc - 898.902;cc + 903;cc - 904.905;cc + 906;cc - 907.1025;cc + 1026;"
                   , "cc - 1027.1080;cc + 1081;cc - 1082.1085;cc + 1086;cc - 1087.1093;cc + 1094;"
                   , "cc - 1095.1110;cc + 1111;cc - 1112.1148;cc + 1149;cc - 1150.1175;cc + 1176;"
                   , "cc - 1177.1178;cc + 1179;cc - 1180.1187;cc + 1188;cc - 1189.1202;cc + 1203;"
                   , "cc - 1204.1205;cc + 1206.1207;cc - 1208.1209;cc + 1210;cc - 1211.1212;"
                   , "cc + 1213;cc - 1214.1215;cc + 1216;cc - 1217.1218;cc + 1219.1220;"
                   , "cc - 1221.1239;cc + 1240;cc - 1241.50227;"
                   , "ccode /4 0;ccode /4 1;ccode /4 2;ccode /4 3;ccode /4 4;ccode /4 5;ccode /4 6;"
                   , "ccode /4 7;ccode /4 8;ccode /4 9;ccode /4 10;ccode /4 11;ccode /4 12;"
                   , "ccode /4 13;ccode /4 14;ccode /4 15;ccode /4 16;ccode /4 17;ccode /4 18;"
                   , "ccode /4 19;ccode /4 20;ccode /4 21;ccode /4 22;ccode /4 23;ccode /4 24;"
                   , "ccode /4 25;ccode /4 26;ccode /4 27;ccode /4 28;ccode /4 29;ccode /4 30;"
                   , "ccode /4 31;ccode /4 32;ccode /4 33;ccode /4 34;ccode /4 35;ccode /4 36;"
                   , "ccode /4 37;ccode /4 38;ccode /4 39;ccode /4 40;ccode /4 41;ccode /4 42;"
                   , "ccode /4 43;ccode /4 44;ccode /4 45;ccode /4 46;ccode /4 47;ccode /4 48;"
                   , "ccode /4 49;ccode /4 50;ccode /4 51;ccode /4 52;ccode /4 53;ccode /4 54;"
                   , "ccode /4 55;ccode /4 56;ccode /4 57;ccode /4 58;ccode /4 59;ccode /4 60;"
                   , "ccode /4 61;ccode /4 62;ccode /4 63;ccode /4 64;ccode /4 65;ccode /4 66;"
                   , "ccode /4 67;ccode /4 68;ccode /4 69;ccode /4 70;ccode /4 71;ccode /4 72;"
                   , "ccode /4 73;ccode /4 74;ccode /4 75;ccode /4 76;ccode /4 77;ccode /4 78;"
                   ]

testCommandCNames :: TestTree
testCommandCNames = testGroup "CNAMES command tests" [cnamesHeader',cnamesStateName',cnamesValidation]
  where
    cnamesHeader' = testGroup "CNAMES command header options" headers
      where
        headers = header <$> (drop 2 $ inits "cnames")
        header str = testCase ("Parses component \"" ++ str ++ "\"") $ parseSuccess cnamesHeader (str ++ " ")
    cnamesStateName' = testGroup "CNAMES naming format" [nameOnly,nameAndStates]
      where
        nameOnly      = testCase "Parses character name only"            $ parseSuccess cnamesStateName "{42 character_name;"
        nameAndStates = testCase "Parses character name and state names" $ parseSuccess cnamesStateName "{15 character_name first_state second_state;"
    cnamesValidation = testGroup "CNAMES validation" [overNamedIndex]
      where
        overNamedIndex = testCase "Multiple names for the same index fails" $ parseFailure cnamesCommand "cn {1 alpha;{1 beta"

testCommandProcedure :: TestTree
testCommandProcedure = testGroup "PROCEDURE command tests" [shortProcedureHeader, longProcedureHeader, closeFilesDirective, commandFile, fastaFile, generalEnding]
  where
    shortProcedureHeader = testCase "Parses component \"proc\""      $ parseSuccess procHeader       "proc "
    longProcedureHeader  = testCase "Parses component \"procedure\"" $ parseSuccess procHeader       "procedure "
    closeFilesDirective  = testCase "Parses component \"/;\""        $ parseSuccess procCloseFile    "/;"
    generalEnding        = testCase "Parses \"proc/;\""              $ parseSuccess procedureCommand "proc /;"
    commandFile = testProperty "parses arbitrary command file reference" f
      where
         f :: NonEmptyList Char -> Bool
         f x = isRight . parse procCommandFile "" $ fileName ++ ";" 
           where fileName = takeWhile (not . isSpace) $ getNonEmpty x
    fastaFile = testProperty "parses arbitrary 'fasta' file reference" f
      where
         f :: NonEmptyList Char -> Bool
         f x = isRight . parse procCommandFile "" $ "&" ++ fileName ++ ";" 
           where fileName = takeWhile (not . isSpace) $ getNonEmpty x

testCommandTRead :: TestTree
testCommandTRead = testGroup "TREAD command tests" [treadHeader',treadLeaf',treadTree']
  where
    treadHeader' = testGroup "TREAD command header options" headers
      where
        headers = header <$> (drop 2 $ inits "tread")
        header str = testCase ("Parses component \"" ++ str ++ "\"") $ parseSuccess treadHeader (str ++ " ")
    treadLeaf' = testGroup "TREAD leaf options" [indexLeaf,prefixLeaf,nameLeaf]
      where
        indexLeaf = testProperty "Index leaf format" f
          where
            f :: NonNegative Int -> Bool
            f n = parse treadLeaf "" (show idx) == Right (Leaf (Index idx))
              where
                idx = getNonNegative n
        prefixLeaf = testProperty "Prefix leaf format" f
          where
            f :: WordToken -> Bool
            f tok = parse treadLeaf "" str == Right (Leaf (Prefix str))
              where
                str = getWordToken tok ++ "..."
        nameLeaf = testProperty "Name leaf format" f
          where
            f :: WordToken -> Bool
            f tok = parse treadLeaf "" str == Right (Leaf (Name str))
              where
                str = getWordToken tok
    treadTree' = testGroup "TREAD tree format" examples
      where
        examples        = makeExample <$> exampleTrees
        makeExample str = testCase ("Parses component \"" ++ str ++ "\"") $ parseSuccess treadTree str
        exampleTrees    = [ "(a)"
                          , "(a(b(c)))"
                          , "((a b)(c d))"
                          ]

testCommandXRead :: TestTree
testCommandXRead = testGroup "XREAD command test" [xreadHeader',xreadDiscreteSequence,xreadDnaSequence,xreadProteinSequence]
  where
    xreadHeader' = testGroup "XREAD header" [beginsWithXREAD, possibleComment]
      where
        beginsWithXREAD = testCase     "Begins with XREAD"           $ parseSuccess xreadHeader "XREAD\n;"
        possibleComment = testProperty "Possibly contains a comment" f
          where
            f :: NonEmptyList Char -> Bool
            f x = isRight $ parse xreadHeader "" input
              where
                input   = "XREAD '" ++ comment ++ "'"
                comment = filter (/= '\'') $ getNonEmpty x
    xreadDiscreteSequence = testGroup "XREAD discrete sequence" [emptyAmbiguityGroup, nonEmptyAmbiguityGroup, onlyDiscreteValues]
      where
        emptyAmbiguityGroup = testProperty "Fails to parse empty ambiguity group" f
          where
            f :: DiscreteCharacters -> Bool
            f dcs = all isLeft $ parse discreteSequence "" <$> opts
              where
                str  = getDiscreteCharacters dcs
                n    = length str
                opts = [ p ++ "[]" ++ s | i <- [0..n], (p,s) <- pure $ splitAt i str ]
        nonEmptyAmbiguityGroup = testProperty "Parses non-empty ambiguity group" f
          where
            f :: (DiscreteCharacters, DiscreteCharacters) -> Bool
            f (dcs,amb) = all isRight $ parse discreteSequence "" <$> opts
              where
                str  = getDiscreteCharacters dcs
                grp  = "[" ++ nub (getDiscreteCharacters amb) ++ "]"
                n    = length str
                opts = [ p ++ grp ++ s | i <- [0..n], (p,s) <- pure $ splitAt i str ]
        onlyDiscreteValues = testProperty "Parses only appropriate characters" f
          where
            f :: WordToken -> Bool
            f tok = (all (`elem`    discreteStateValues) str && isRight res)
                 || (any (`notElem` discreteStateValues) str && isLeft  res)
              where
                res  = parse (discreteSequence <* eof) "" str
                str  = getWordToken tok
    xreadDnaSequence = testGroup "XREAD dna sequence" [onlyDnaValues]
      where
        onlyDnaValues = testProperty "Parses only appropriate characters" f
          where
            f :: WordToken -> Bool
            f tok = (all (`elem`    dnaStateValues) str && isRight res)
                 || (any (`notElem` dnaStateValues) str && isLeft  res)
              where
                res  = parse (dnaSequence <* eof) "" str
                str  = getWordToken tok
    xreadProteinSequence = testGroup "XREAD protein sequence" [onlyProteinValues]
      where
        onlyProteinValues = testProperty "Parses only appropriate characters" f
          where
            f :: WordToken -> Bool
            f tok = (all (`elem`    proteinStateValues) str && isRight res)
                 || (any (`notElem` proteinStateValues) str && isLeft  res)
              where
                res  = parse (proteinSequence <* eof) "" str
                str  = getWordToken tok
        
        

newtype DiscreteCharacters = DC (NonEmpty Char) deriving (Eq,Show)

instance Arbitrary DiscreteCharacters where
  arbitrary = fmap (DC . NE.fromList) . listOf1 . elements $ toList discreteStateValues

getDiscreteCharacters (DC x) = toList x

powerSet :: [a] -> [[a]]
powerSet = filterM (const [False,True])
