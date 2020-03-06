{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import Benchmark.StringAlignment
--import Bio.Metadata.CharacterName
import Bio.Metadata.Dynamic
import Control.Lens
--import Data.Char
import Data.List
import Data.MonoTraversable


main :: IO ()
main = let p = (128, 144)
       in  benchStringAlignment $
             \(m, x, y) -> ("two-hex-pref-gap" `isInfixOf` otoList (m ^. characterName))
{-
       in  benchStringAlignment $
             \(m, x, y) -> ((olength x == fst p && olength y == snd p)
                        ||  (olength x == snd p && olength y == fst p))
                        && not ("pref-sub" `isInfixOf` otoList (m ^. characterName))
-}
