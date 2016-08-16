-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.InsertionEvents.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Unit tests for the InvertionEvents type
-----------------------------------------------------------------------------

module Analysis.ImpliedAlignment.InsertionEvents.Test (testSuite) where

import           Analysis.ImpliedAlignment.InsertionEvents.Internal
import qualified Analysis.ImpliedAlignment.DeletionEvents as DE
import Data.Monoid
import Test.QuickCheck.Property.Common.Internal (Equal,runEqual)
import Test.QuickCheck.Property.Monoid
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testSuite :: TestTree
testSuite = testGroup "Insertion Event operations"
    [ testMonoidOperator
    , testCoalesce
    ]

testMonoidOperator :: TestTree
testMonoidOperator = testGroup "The monoid operator behaves as expected"
    [ monoidOperatorTestCases
    , monoidProperties
    ]

monoidProperties :: TestTree
monoidProperties = testProperty "InsertionEvents is a proper Monoid instance" f
  where
    f :: InsertionEvents Char Int -> InsertionEvents Char Int -> InsertionEvents Char Int -> Bool
    f x y z = runEqual (==) $ prop e x y z
--    e :: T (InsertionEvents Char)
    e = T
    prop :: (Eq a, Eq e) => T (InsertionEvents a e) -> InsertionEvents a e -> InsertionEvents a e -> InsertionEvents a e -> Equal (InsertionEvents a e)
    prop = prop_Monoid

monoidOperatorTestCases :: TestTree
monoidOperatorTestCases = testGroup "Monoid operator test cases"
    [ testCase "Test left bias"   $ fromList [(1,"AB")]                        @=? fromList [(1,"A")]         <> fromList [(1,"B")]
    , testCase "Test left bias 2" $ fromList [(1,"ACDC")]                      @=? fromList [(1,"AC")]        <> fromList [(1,"DC")]
    , testCase "Test interlacing" $ fromList [(1,"A"),(2,"B"),(3,"C"),(4,"D")] @=? fromList [(1,"A"),(3,"C")] <> fromList [(2,"B"),(4,"D")]
    ]

testCoalesce :: TestTree
testCoalesce = testGroup "The coalesce function behaves as expected"
    [ coalesceTestCases
    ]

{-
coalesceOperatorTestCases :: TestTree
coalesceOperatorTestCases = testGroup "Coalesce operator test cases" 
    [ testCase "Test decrementing  case"    $ fromList [(1,"A"),(2,"B")]       @=? fromList [(1,"A")]          <^> fromList [(3,"B")]
    , testCase "Test appending     case"    $ fromList [(1,"ABCD" )]           @=? fromList [(1,"A")]          <^> fromList [(2,"BCD")]
    , testCase "Test prepending    case"    $ fromList [(1,"ABCD" )]           @=? fromList [(1,"BCD")]        <^> fromList [(1,"A")]
    , testCase "Test inserting     case"    $ fromList [(1,"ABCDE")]           @=? fromList [(1,"ABDE")]       <^> fromList [(3,"C")]
    , testCase "Test all three     case"    $ fromList [(1,"ABCDE")]           @=? fromList [(1,"BD")]         <^> fromList [(1,"A"),(2,"C"),(3,"E")]
    , testCase "Test padded middle case" $ fromList [(1,"AB"),(2,"X"),(3,"CD")] @=? fromList [(1,"B"),(2,"X"),(3,"C")] <^> fromList [(1,"A"),(6,"D")]
    , testCase "Test double case"        $ fromList [(1,"AB"),(2,"CD")]     @=? fromList [(1,"A"),(2,"D")]  <^> fromList [(2,"B"),(3,"C")]
    , testCase "Test all of above case"  $ fromList [(1,"ABCDE"),(2,"MN"),(3,"XYZ")] @=? fromList [(1,"BD"),(2,"MN"),(3,"Y")] <^> fromList [(1,"A"),(2,"C"),(3,"E"),(7,"X"),(8,"Z")]
    ]
-}

coalesceTestCases :: TestTree
coalesceTestCases = testGroup "Coalesce function test cases"
    [ coalesceInitialTestCases
    , coalesceSecondaryTestCases
    ]

coalesceSecondaryTestCases :: TestTree
coalesceSecondaryTestCases = testGroup "Secondary test cases"
    [ testCase "Test leading decendant insertion" $ fromList [(0,"V" ),(1,"ABCDE"),(2,"MN"),(3,"XYZ")]      @=? coalesce  mempty           (fromList [(1,"BD"),(2,"MN"),(3,"Y")])               [fromList [(0,"V"),(1,"A"),(2,"C"),(3,"E"),(7,"X"),(8,"Z")]]
    , testCase "Test lets try a deletion"         $ fromList [(2,"A" ),(3,"C"),(4,"E")]                     @=? coalesce (DE.fromList [0])  mempty                                               [fromList [(1,"A"),(2,"C"),(3,"E")]]
    , testCase "Test lets try a deletion 2"       $ fromList [(1,"XA"),(2,"C"),(3,"E")]                     @=? coalesce (DE.fromList [0]) (fromList [(1,"X")])                                  [fromList [(1,"A"),(2,"C"),(3,"E")]]
    , testCase "Test (22,9)"                      $ fromList [(2,"I" ),(3,"I"),(5,"I"),(6,"II"),(7,"IXYZ")] @=? coalesce (DE.fromList [0]) (fromList [(2,"I"),(3,"I"),(5,"I"),(6,"II"),(7,"I")]) [fromList [(12,"XYZ")]]
    ]

coalesceInitialTestCases :: TestTree
coalesceInitialTestCases = testGroup "Initial test cases"
    [ testCase "Test decrementing  case" $ fromList [(1,"A"),(2,"B")]                @=? coalesce mempty (fromList [(1,"A")   ])                [fromList [(3,"B")  ]]
    , testCase "Test appending     case" $ fromList [(1,"ABCD" )]                    @=? coalesce mempty (fromList [(1,"A")   ])                [fromList [(2,"BCD")]]
    , testCase "Test prepending    case" $ fromList [(1,"ABCD" )]                    @=? coalesce mempty (fromList [(1,"BCD") ])                [fromList [(1,"A")  ]]
    , testCase "Test inserting     case" $ fromList [(1,"ABCDE")]                    @=? coalesce mempty (fromList [(1,"ABDE")])                [fromList [(3,"C")  ]]
    , testCase "Test all three     case" $ fromList [(1,"ABCDE")]                    @=? coalesce mempty (fromList [(1,"BD")  ])                [fromList [(1,"A"),(2,"C"),(3,"E")]]
    , testCase "Test padded middle case" $ fromList [(1,"AB"),(2,"X"),(3,"CD")]      @=? coalesce mempty (fromList [(1,"B"),(2,"X"),(3,"C")])   [fromList [(1,"A"),(6,"D")]]
    , testCase "Test double        case" $ fromList [(1,"AB"),(2,"CD")]              @=? coalesce mempty (fromList [(1,"A"),(2,"D")])           [fromList [(2,"B"),(3,"C")]]
    , testCase "Test all of above  case" $ fromList [(1,"ABCDE"),(2,"MN"),(3,"XYZ")] @=? coalesce mempty (fromList [(1,"BD"),(2,"MN"),(3,"Y")]) [fromList [(1,"A"),(2,"C"),(3,"E"),(7,"X"),(8,"Z")]]
    ]
