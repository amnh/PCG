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

import Analysis.ImpliedAlignment.InsertionEvents.Internal
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit

testSuite :: TestTree
testSuite = testGroup "Insertion Event operations"
    [ testMonoidOperator
    , testCoalesceOperator
    ]

testMonoidOperator :: TestTree
testMonoidOperator = testGroup "The monoid operator behaves as expected"
    [ monoidOperatorTestCases
    ]

monoidOperatorTestCases :: TestTree
monoidOperatorTestCases = testGroup "Monoid operator test cases"
    [ testCase "Test left bias"   $ fromList [(1,"AB")]                        @=? fromList [(1,"A")]         <> fromList [(1,"B")]
    , testCase "Test left bias 2" $ fromList [(1,"ACDC")]                      @=? fromList [(1,"AC")]        <> fromList [(1,"DC")]
    , testCase "Test interlacing" $ fromList [(1,"A"),(2,"B"),(3,"C"),(4,"D")] @=? fromList [(1,"A"),(3,"C")] <> fromList [(2,"B"),(4,"D")]
    ]

testCoalesceOperator :: TestTree
testCoalesceOperator = testGroup "The coalesce operator behaves as expected"
    [ coalesceOperatorTestCases
    ]

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
