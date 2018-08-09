module Main ( main ) where


import qualified Data.BitMatrix.Test             as BitMatrix
import qualified Data.List.Test                  as DataList
import qualified Data.MutualExclusionSet.Test    as MutualExclusionSet
import qualified Numeric.Extended.Natural.Test   as ExtendedNatural
import qualified Numeric.Extended.Real.Test      as ExtendedReal
import qualified Numeric.NonNegativeAverage.Test as NonNegativeAverage

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun    (rerunningTests)


main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [ BitMatrix.testSuite
    , ExtendedNatural.testSuite
    , ExtendedReal.testSuite
    , NonNegativeAverage.testSuite
    , MutualExclusionSet.testSuite
    , DataList.testSuite
    ]
