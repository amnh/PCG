{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Binary.SequentialAlign (sequentialAlign) where

import           Bio.Sequence.Coded
import qualified Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI as FF (sequentialAlign)
import           Data.Foldable
import           Data.List.Split (chunksOf)

-- | sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
-- It gets called from Analysis.Parsimony.Binary.Optimization:preorderNodeOptimize
-- the 's' in the input type is a Vector of Maybe Vector of *
-- The particular version of SeqConstraint used here is found in Analysis.Parsimony.Binary.Internal
-- and has these constraints: (CodedSequence s b, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
-- 
sequentialAlign :: (CodedSequence s) => s -> s -> (s, Double, s, s, s)
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
        (cost, alignment1, alignment2) = case FF.sequentialAlign 1 1 inpSeq1' inpSeq2' of
            Left e -> error e -- Better error handling later
            Right r -> r
        alphabet = ["A", "C", "G", "T", "-"]
        encodeStr x = encodeOverAlphabet (pure (chunksOf 1 x)) alphabet
        decodeStr x = concat . concat . toList $ decodeOverAlphabet x alphabet
