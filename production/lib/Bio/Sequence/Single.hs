-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
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

module Bio.Sequence.Single where

-- | A single sequence may have no ambiguity, so it's capable of checking that condition
class SingleSequence s where
    checkAmbiguous :: s -> Bool