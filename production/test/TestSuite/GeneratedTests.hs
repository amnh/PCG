module TestSuite.GeneratedTests
  ( testSuite
  ) where

import           Test.Tasty                              (TestTree,testGroup)
import qualified TestSuite.GeneratedTests.Fasta as Fasta (testSuite)
import qualified TestSuite.GeneratedTests.Fastc as Fastc (testSuite)
import qualified TestSuite.GeneratedTests.Nexus as Nexus (testSuite)
<<<<<<< HEAD
-- import qualified TestSuite.GeneratedTests.TNT   as TNT   (testSuite)
=======
--import qualified TestSuite.GeneratedTests.TNT   as TNT   (testSuite)
>>>>>>> 1b33e38264bc602000972626289bc06e1cd90653

testSuite :: IO TestTree
testSuite = testGroup "Dynamically generated tests" 
           <$> sequence 
             [ Fasta.testSuite
             , Fastc.testSuite
             , Nexus.testSuite
<<<<<<< HEAD
             -- , TNT.testSuite
=======
--             , TNT.testSuite
>>>>>>> 1b33e38264bc602000972626289bc06e1cd90653
             ]
