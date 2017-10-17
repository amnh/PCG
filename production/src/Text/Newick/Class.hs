---------------------------------------------------------------------------
-- |
-- Module      :  Text.Newick.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
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


-- |
class ToNewick a where

    toNewick :: a -> String


