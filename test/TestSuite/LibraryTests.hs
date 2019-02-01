module TestSuite.LibraryTests
  ( testSuite
  ) where

import qualified Bio.Character.Encodable.Dynamic.Test as DynamicChar
import qualified Bio.Character.Encodable.Static.Test  as StaticChar
import qualified Bio.Graph.ReferenceDAG.Test          as ReferenceDAG
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [ DynamicChar.testSuite
    , StaticChar.testSuite
    , ReferenceDAG.testSuite
    ]
