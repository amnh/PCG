-----------------------------------------------------------------------------
-- |
-- Module      :  TextShow.Custom
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for working with TextShow and Builder
--
-----------------------------------------------------------------------------


module TextShow.Custom
  ( intercalateB
  ) where


import Data.Foldable (Foldable (fold))
import Data.List     (intersperse)
import TextShow      (Builder)

intercalateB :: Builder -> [Builder] -> Builder
intercalateB sep bss = fold (intersperse sep bss)
