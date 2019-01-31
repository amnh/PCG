module TestSuite.LibraryTests
  ( testSuite
  ) where

import qualified Bio.Character.Encodable.Dynamic.Test as DynamicChar
import qualified Bio.Character.Encodable.Static.Test  as StaticChar
import qualified Bio.Graph.ReferenceDAG.Test          as ReferenceDAG
import qualified Control.Evaluation.Test              as Evaluation
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [ Evaluation.testSuite
    , DynamicChar.testSuite
    , StaticChar.testSuite
    , Pairwise.testSuite
    , ReferenceDAG.testSuite
    ]
