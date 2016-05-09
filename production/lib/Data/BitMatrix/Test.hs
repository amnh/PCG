{-# LANGUAGE FlexibleInstances #-}

module Bio.Data.BitMatrix
  ( testSuite
  ) where

import Bio.Data.BitMatrix
import Data.Bits
import Data.BitVector
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "BitMatrix tests" [testGeneratingFn]

testGeneratingFn :: TestTree
testGeneratingFn = testGroup "BitMatrix generating fn"
        alphLen <- getPositive <$> (arbitrary :: Gen (Positive Int))
        numChars <- getPositive <$> (arbitrary :: Gen (Positive Int))
        let testBM = bitMatrix numChars alphLen $ const True
        controlBM  = bitVec (alphLen * numChars) (2 ^ (alphLen * numChars) - 1)
