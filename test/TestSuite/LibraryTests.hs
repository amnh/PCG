module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where


import qualified Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test as Pairwise
import qualified Bio.Character.Encodable.Dynamic.Test                        as DynamicChar
import qualified Bio.Character.Encodable.Static.Test                         as StaticChar
import qualified Control.Evaluation.Test                                     as Evaluation
--import qualified Test.Custom.Tree.Test                             as MockTree

import           Test.Tasty


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
    [ Evaluation.testSuite
    , DynamicChar.testSuite
    , StaticChar.testSuite
    , Pairwise.testSuite
--    , MockTree.testSuite
    ]

