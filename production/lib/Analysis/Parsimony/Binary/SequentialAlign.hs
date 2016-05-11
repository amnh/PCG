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
import qualified Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI as FFI (sequentialAlign)
import           Bio.Character.Dynamic.Coded
import           Data.Alphabet
import           Data.Vector (fromList)
import           Data.Foldable
import           Data.List.Split (chunksOf)

-- | sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
-- It gets called from Analysis.Parsimony.Binary.Optimization:preorderNodeOptimize
-- The particular version of SeqConstraint used here is found in Analysis.Parsimony.Binary.Internal
-- and has these constraints: (EncodableDynamicCharacter s b, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
--
sequentialAlign :: (EncodableDynamicCharacter s) => s -> s -> (s, Double, s, s, s)
sequentialAlign inpSeq1 inpSeq2 = (inferredParent', fromIntegral cost :: Double, alignedParent', alignment1', alignment2')
    where
        (inferredParent, alignedParent) = foldr (\(x, y) acc -> createParentSeqs x y acc) ([],[]) (zip alignment1 alignment2)
        createParentSeqs x y (xs, ys)
            | x == '-' && y == '-' = (xs    , '-' : ys)
            | x == '-'             = (xs, x : ys) -- So I'm prioritizing gap over everything else
            | y == '-'             = (xs, y : ys) -- Note that '-' comes before letters alphabetically,
                                                  -- but I still have to do this to deal with gap removal in inferred Parent
            | x < y                = (x : xs, x : ys)
            | y < x                = (y : xs, y : ys)
            | otherwise            = (x : xs, x : ys) -- they must be equal, so choose x
        inferredParent' = encodeStr inferredParent
        alignedParent'  = encodeStr alignedParent
        alignment1'     = encodeStr alignment1
        alignment2'     = encodeStr alignment2
        inpSeq1'        = decodeStr inpSeq1
        inpSeq2'        = decodeStr inpSeq2
        (cost, alignment1, alignment2) = case FFI.sequentialAlign 1 1 inpSeq1' inpSeq2' of
            Left e -> error e -- TODO: Better error handling later
            Right r -> r
        alphabet = Alphabet' $ fromList ["A", "C", "G", "T", "-"] -- TODO: Eventually this shouldn't be hard-coded.
        encodeStr x = encodeDynamic alphabet [chunksOf 1 x]
        decodeStr x = concat . concat . toList $ decodeDynamic alphabet x
