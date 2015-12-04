module TestSuite.GeneratedTests
  ( testSuite
  ) where

import           Test.Tasty                              (TestTree,testGroup)
import qualified TestSuite.GeneratedTests.Fasta as Fasta (testSuite)
import qualified TestSuite.GeneratedTests.Nexus as Nexus (testSuite)

testSuite :: IO TestTree
testSuite = testGroup "Dynamically generated tests" 
           <$> sequence 
             [ Fasta.testSuite
             , Nexus.testSuite
             ]
