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

import           Data.Vector     (Vector)
import qualified Data.Vector     as V (fromList)
import           Test.QuickCheck


-- | A 'Vector' of arbitrary length (possibly empty) containing arbitrary values of the paramaterized type.
instance Arbitrary a => Arbitrary (Vector a) where

    arbitrary = V.fromList <$> listOf arbitrary
