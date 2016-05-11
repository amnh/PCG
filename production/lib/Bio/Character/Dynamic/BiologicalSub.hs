-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.BiologicalSub
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for a sequence that is not allowed to have ambiguity
--
-----------------------------------------------------------------------------

module Bio.Character.Dynamic.BiologicalSub where

-- | A sub-dynamic character may have no ambiguity, so it's capable of checking that condition
class BiologicalSub s where
    checkAmbiguous :: s -> Bool
