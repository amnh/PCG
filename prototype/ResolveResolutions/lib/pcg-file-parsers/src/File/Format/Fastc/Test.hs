
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fastc.Test
  ( testSuite
  ) where

import Control.Arrow            (first,second)
import Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList)
import Data.Vector              (Vector,fromList)
import File.Format.Fasta.Test   (validTaxonLines)
import File.Format.Fastc.Parser
import Test.Custom.Parse        (parseEquals)
import Test.Tasty               (TestTree,testGroup)
import Test.Tasty.HUnit


testSuite :: TestTree
testSuite = testGroup "Fastc Format"
    [ testGroup "Fastc Parser"
        [fastcSymbolSequence',fastcTaxonSequenceDefinition',fastcStreamParser']
    ]


fastcSymbolSequence' :: TestTree
fastcSymbolSequence' = testGroup "fastcSymbolSequence" [valid]
  where
    f (res,str) = testCase (show str) $ parseEquals fastcSymbolSequence str res
    valid       = testGroup "Valid sequences" $ f <$> validSequences


validSequences :: [(Vector (NonEmpty String), String)]
validSequences = first (fromList . fmap NE.fromList) <$>
    [ ([["wow"]]                                           , "wow\n"                         )
    , ([["wow"],["such"]]                                  , "wow such\n"                    )
    , ([["wow"],["such"],["very"]]                         , " wow such very \n"             )
    , ([["wow"],["such","very","much"],["success"]]        , "wow such | very | much success\n")
    , ([["wow"],["such","very"],["success"],["many","much"]
       ,["compile"],["so","amaze"],["parse"],["wow"]]      , "wow such|very success many|much compile so|amaze parse wow\n")
    ]


fastcTaxonSequenceDefinition' :: TestTree
fastcTaxonSequenceDefinition' = testGroup "fastaTaxonSequenceDefinition" [valid]
  where
    f (res,str) = testCase (show str) $ parseEquals fastcTaxonSequenceDefinition str res
    valid               = testGroup "Valid sequences" $ f <$> validTaxonSequences


validTaxonSequences :: [(FastcSequence,String)]
validTaxonSequences = zipWith f validTaxonLines validSequences
  where
    f (x,str) (y,seq')  = (FastcSequence x y, concat [str,"\n",seq'])


fastcStreamParser' :: TestTree
fastcStreamParser' = testGroup "fastaStreamParser" [testGroup "Valid stream" [validStream]]
  where
    validStream = testCase "Fastc concatenated stream" $ parseEquals fastcStreamParser str (NE.fromList res)
    (res,str)   = second concat $ unzip validTaxonSequences

