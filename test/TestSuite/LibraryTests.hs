module TestSuite.LibraryTests
  ( testSuite
  ) where

import qualified Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test as Pairwise
import qualified Bio.Character.Encodable.Dynamic.Test                        as DynamicChar
import qualified Bio.Character.Encodable.Static.Test                         as StaticChar
import qualified Bio.Graph.ReferenceDAG.Test                                 as ReferenceDAG
import qualified Control.Evaluation.Test                                     as Evaluation
import qualified Data.Alphabet.Test                                          as Alphabet
import qualified Data.TCM.Test                                               as TCM
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [
 --     Evaluation.testSuite
 --   , Alphabet.testSuite
 --   , TCM.testSuite
 --   , DynamicChar.testSuite
 --   , StaticChar.testSuite
 --   , Pairwise.testSuite
      ReferenceDAG.testSuite
    ]
