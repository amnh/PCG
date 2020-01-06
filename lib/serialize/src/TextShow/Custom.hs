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

import Data.Foldable
import Data.List     (intersperse)
import TextShow      (Builder)


-- |
-- Place a copy of the builder element in between each element in the foldable
-- structure and treuct the new builder.
intercalateB :: Foldable f => Builder -> f Builder -> Builder
intercalateB sep = fold . intersperse sep . toList
