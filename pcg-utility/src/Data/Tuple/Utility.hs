-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions for pairs
--
-----------------------------------------------------------------------------

module Data.Tuple.Utility
  ( proj3_1
  , proj3_2
  , proj3_3
  ) where

-- |
-- Project from triple onto first component.
proj3_1 :: (a,b,c) -> a
proj3_1 (a, _, _) = a

-- |
-- Project from triple onto second component.
proj3_2 :: (a,b,c) -> b
proj3_2 (_, b, _) = b

-- |
-- Project from triple onto third component.
proj3_3 :: (a,b,c) -> c
proj3_3 (_, _, c) = c
