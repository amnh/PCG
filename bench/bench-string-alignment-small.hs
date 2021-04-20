------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import Benchmark.StringAlignment
import Bio.Metadata.Dynamic
import Control.Lens
import Data.List                 (isInfixOf)
import Data.MonoTraversable


-- |
-- Entry point for the /short/ string alignment benchmarks.
main :: IO ()
main = benchStringAlignment $
            \(m, _, _) -> "two-hex-pref-gap" `isInfixOf` otoList (m ^. characterName)
{-
       let p = (128, 144)
       in  benchStringAlignment $
             \(m, x, y) -> ((olength x == fst p && olength y == snd p)
                        ||  (olength x == snd p && olength y == fst p))
                        && not ("pref-sub" `isInfixOf` otoList (m ^. characterName))
-}
