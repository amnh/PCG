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

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Analysis.ImpliedAlignment.InsertionEvents.Test where

import Analysis.ImpliedAlignment.InsertionEvents.Internal
import Data.Monoid
--import           Test.Custom
import Test.Tasty
import Test.Tasty.HUnit
--import           Test.Tasty.QuickCheck
--import           Test.QuickCheck.Arbitrary.Instances

--import Debug.Trace

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
    [ testCase "Test appending  case" $ fromList [(1,"ABCD" )] @=? fromList [(1,"A")]    <^> fromList [(2,"BCD")]
    , testCase "Test prepending case" $ fromList [(1,"ABCD" )] @=? fromList [(1,"BCD")]  <^> fromList [(1,"A")]
    , testCase "Test inserting  case" $ fromList [(1,"ABCDE")] @=? fromList [(1,"ABDE")] <^> fromList [(4,"C")]
    , testCase "Test all three  case" $ fromList [(1,"ABCDE")] @=? fromList [(1,"BD")]   <^> fromList [(1,"A"),(2,"C"),(3,"E")]
    ]
