module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where

import qualified Control.Evaluation.Test              as Evaluation
import qualified Bio.Character.Encodable.Dynamic.Test as DynamicChar
import qualified Bio.Character.Encodable.Static.Test  as StaticChar
import qualified Data.BitMatrix.Test                  as BitMatrix
import qualified Numeric.Extended.Natural.Test        as ExtendedNatural
import qualified Numeric.Extended.Real.Test           as ExtendedReal
import qualified Numeric.NonNegativeAverage.Test      as NonNegativeAverage
--import qualified Test.Custom.Tree.Test                             as MockTree

import           Test.Tasty

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
    [ Evaluation.testSuite
    , DynamicChar.testSuite
    , StaticChar.testSuite
    , BitMatrix.testSuite
    , ExtendedNatural.testSuite
    , ExtendedReal.testSuite
    , NonNegativeAverage.testSuite
--    , MockTree.testSuite
    ]

