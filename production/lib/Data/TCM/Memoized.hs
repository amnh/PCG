-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM.Memoized
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.TCM.Memoized
  ( FFI.MemoizedCostMatrix
  , generateMemoizedCostMatrix
  ) where

import qualified Data.TCM.Memoized.FFI as FFI

generateMemoizedCostMatrix :: Word -> (Word -> Word -> Word) -> FFI.MemoizedCostMatrix
generateMemoizedCostMatrix = FFI.getMemoizedCostMatrix
