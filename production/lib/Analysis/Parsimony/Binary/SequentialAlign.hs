-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.SequentialAlign
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module exposing an alignment optimization from Yu Xiang's research at Harvard.
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Binary.SequentialAlign (sequentialAlign) where

--import           Analysis.Parsimony.Binary.Internal
import qualified Analysis.Parsimony.Binary.SequentialAlign.FFI as FFI (sequentialAlign)
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Exportable.Class
import           Data.Alphabet
import           Data.Vector     (fromList)
import           Data.Foldable
import           Data.List.Split (chunksOf)
import           Data.MonoTraversable

-- | sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
-- It gets called from Analysis.Parsimony.Binary.Optimization:preorderNodeOptimize
-- The particular version of SeqConstraint used here is found in Analysis.Parsimony.Binary.Internal
-- and has these constraints: (EncodableDynamicCharacter s b, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
--
sequentialAlign :: (EncodableDynamicCharacter s, Exportable s) => s -> s -> (s, Double, s, s, s)
sequentialAlign inpSeq1 inpSeq2 = ( constructDynamic inferredParent
                                  , fromIntegral cost :: Double
                                  , constructDynamic alignedParent
                                  , alignment1
                                  , alignment2
                                  )
    where
      (cost, alignment1, alignment2) =
        case FFI.sequentialAlign 1 1 inpSeq1 inpSeq2 of
          Left  e -> error e -- TODO: Better error handling later
          Right r -> r

      (inferredParent, alignedParent) = foldr (\(x, y) acc -> createParentSeqs x y acc) ([],[])
                                      $ zip (otoList alignment1) (otoList alignment2)
      createParentSeqs x y (xs, ys)
        | x == gap && y == gap = (xs    , gap : ys)
        | x == gap             = (xs, x : ys) -- So I'm prioritizing gap over everything else
        | y == gap             = (xs, y : ys) -- Note that '-' comes before letters alphabetically,
                                              -- but I still have to do this to deal with gap removal in inferred Parent
--        | x < y                = (x : xs, x : ys)
--        | y < x                = (y : xs, y : ys)
        | otherwise            = (x : xs, x : ys) -- they must be equal, so choose x
--      inferredParent' = constructDynamic inferredParent
      gap = getGapChar $ inpSeq1 `indexChar` 0
