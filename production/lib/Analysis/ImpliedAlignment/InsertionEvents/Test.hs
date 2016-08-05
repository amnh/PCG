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
    ]

testMonoidOperator :: TestTree
testMonoidOperator = testGroup "The monoid operator behaves as expected"
    [ monoidOperatorTestCases
    ]

monoidOperatorTestCases :: TestTree
monoidOperatorTestCases = testGroup "Monoif operator test cases"
    [ testLeftBias
    ]

testLeftBias :: TestTree
testLeftBias = testCase "TestLeftBias" $
    fromList [(1,"AB")] @=? fromList [(1,"A")] <> fromList [(1,"B")]
