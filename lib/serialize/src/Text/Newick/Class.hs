---------------------------------------------------------------------------
-- |
-- Module      :  Text.Newick.Class
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Output tree structure as Newick or eNewick code.
--
-----------------------------------------------------------------------------


module Text.Newick.Class where

import Data.Text

-- |
-- Render a type to a "Newick string."
class ToNewick a where

    toNewick :: a -> Text
