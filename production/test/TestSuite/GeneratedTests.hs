module TestSuite.GeneratedTests
  ( testSuite
  ) where

import           Test.Tasty                              (TestTree,testGroup)
import qualified TestSuite.GeneratedTests.Fasta as Fasta (testSuite)
import qualified TestSuite.GeneratedTests.Fastc as Fastc (testSuite)
import qualified TestSuite.GeneratedTests.Nexus as Nexus (testSuite)
--import qualified TestSuite.GeneratedTests.TNT   as TNT   (testSuite)

testSuite :: IO TestTree
testSuite = testGroup "Dynamically generated tests" 
           <$> sequence 
             [ Fasta.testSuite
             , Fastc.testSuite
             , Nexus.testSuite
             -- , TNT.testSuite
             ]
