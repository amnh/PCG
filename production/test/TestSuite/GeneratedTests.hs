module TestSuite.GeneratedTests
  ( testSuite
  ) where

import           Test.Tasty                              (TestTree,testGroup)
import qualified TestSuite.GeneratedTests.Fasta as Fasta (testSuite)

testSuite :: IO TestTree
testSuite = testGroup "Dynamically generated tests" 
           <$> sequence 
             [ Fasta.testSuite
             ]
