-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Arbitrary.Instances
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A module to store orphan 'Arbitrary' instances.
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Arbitrary.Instances where

import           Data.IntSet      (IntSet)
import qualified Data.IntSet as IS
import           Data.Vector      (Vector)
import qualified Data.Vector as V (fromList)
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = V.fromList <$> listOf arbitrary

instance Arbitrary IntSet where
    arbitrary = IS.fromList <$> listOf arbitrary