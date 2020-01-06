{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Fastc.Test
  ( testSuite
  ) where

import           Control.Arrow            (first, second)
import           Data.Foldable
import qualified Data.List.NonEmpty       as NE (fromList)
import           Data.String
import           Data.Text.Short          (ShortText)
import           Data.Vector.NonEmpty     (Vector, fromNonEmpty)
import           File.Format.Fasta.Test   (validTaxonLines)
import           File.Format.Fastc.Parser
import           Test.Custom.Parse        (parseEquals)
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit


testSuite :: TestTree
testSuite = testGroup "Fastc Format"
    [ testGroup "Fastc Parser"
        [fastcSymbolSequence',fastcTaxonSequenceDefinition',fastcStreamParser']
    ]


fastcSymbolSequence' :: TestTree
fastcSymbolSequence' = testGroup "fastcSymbolSequence" [valid]
  where
    f (res, str) = testCase (show str) $ parseEquals fastcSymbolSequence str res
    valid        = testGroup "Valid sequences" $ f <$> validSequences


validSequences :: [([Vector ShortText], String)]
validSequences = first toCharSeq <$>
    [ ([["wow"]]                                           , "wow\n"                         )
    , ([["wow"],["such"]]                                  , "wow such\n"                    )
    , ([["wow"],["such"],["very"]]                         , " wow such very \n"             )
    , ([["wow"],["such","very","much"],["success"]]        , "wow [such very much] success\n")
    , ([["wow"],["such","very"],["success"],["many","much"]
       ,["compile"],["so","amaze"],["parse"],["wow"]]      , "wow [such very] success [many much] compile [so amaze] parse wow\n")
    ]
  where
    toCharSeq :: [[String]] -> [Vector ShortText]
    toCharSeq = fmap (fmap fromString . vecFromList)


fastcTaxonSequenceDefinition' :: TestTree
fastcTaxonSequenceDefinition' = testGroup "fastaTaxonSequenceDefinition" [valid]
  where
    f (res,str) = testCase (show str) $ parseEquals fastcTaxonSequenceDefinition str res
    valid               = testGroup "Valid sequences" $ f <$> validTaxonSequences


validTaxonSequences :: [(FastcSequence, String)]
validTaxonSequences = zipWith f validTaxonLines validSequences
  where
    f (x, str) (y, seq') =
        let a = FastcSequence x $ vecFromList y
            b = fold [ str, "\n", seq' ]
        in  (a, b)


fastcStreamParser' :: TestTree
fastcStreamParser' = testGroup "fastaStreamParser" [testGroup "Valid stream" [validStream]]
  where
    validStream = testCase "Fastc concatenated stream" $ parseEquals fastcStreamParser str (NE.fromList res)
    (res,str)   = second concat $ unzip validTaxonSequences


vecFromList :: [a] -> Vector a
vecFromList = fromNonEmpty . NE.fromList
