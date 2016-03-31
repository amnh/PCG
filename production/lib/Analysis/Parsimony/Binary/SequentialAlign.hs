{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}



module Analysis.Parsimony.Binary.SequentialAlign (sequentialAlign) where

import           Analysis.Parsimony.Binary.Internal
--import           Bio.Phylogeny.Tree.Node.Preliminary
import           Bio.Sequence.Coded
import qualified Analysis.Parsimony.Binary.SequentialAlign.SeqAlignFFI as FF (sequentialAlign)
--import           Control.Applicative        (liftA2)
import           Data.Bits
import           Data.Foldable              (minimumBy)
--import           Data.Function              (on)
import           Data.List                  (elemIndex)
import           Data.List.Split            (chunksOf)
import           Data.Matrix.NotStupid      (fromList, Matrix, (<->), nrows, ncols, getElem, zero, matrix)
import           Data.Maybe
--import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import           Data.Vector                (Vector, singleton, length, cons, empty, toList, (!), ifoldr)
import qualified Data.Vector as V           (concat, fromList)
import           Prelude             hiding (length, zipWith, or)

import Data.Monoid ((<>))

import           Debug.Trace

-- All this for the stupid parsing we have to do since the bits are inaccessible.
import Data.Functor (($>))
import Data.Bits
import Text.Megaparsec
import Text.Megaparsec.Prim (MonadParsec)
import Text.Megaparsec.Lexer (integer)

data Direction = LeftDir | DiagDir | DownDir deriving (Eq, Show)

type AlignRow s = (Vector Float, s, Vector Direction)
data AlignMatrix s = AlignMatrix {costs :: Matrix Float, seqs :: Vector s, traversal :: Matrix Direction} deriving (Eq, Show)

type Costs = (Float, Float)

-- | sequentialAlign is similar to DO, but uses Yu's and Vahid's information theoretical sequential alignment algorithm to produce the alignment
-- It gets called from Analysis.Parsimony.Binary.Optimization:preorderNodeOptimize
-- the 's' in the input type is a Vector of Maybe Vector of *
-- The particular version of SeqConstraint used here is found in Analysis.Parsimony.Binary.Internal
-- and has these constraints: (CodedSequence s b, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
-- 
sequentialAlign :: (CodedSequence s) => s -> s -> (s, Double, s, s, s)
sequentialAlign inpSeq1 inpSeq2 = (inferredParent', (fromIntegral cost :: Double), alignedParent', alignment1', alignment2')
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
        inferredParent' = encode inferredParent
        alignedParent'  = encode alignedParent
        alignment1'     = encode alignment1
        alignment2'     = encode alignment2
        inpSeq1'        = decode inpSeq1
        inpSeq2'        = decode inpSeq2
        (cost, alignment1, alignment2) = case FF.sequentialAlign 1 1 inpSeq1' inpSeq2' of
            Left e -> error e -- Better error handling later
            Right r -> r
        alphabet = ["A", "C", "G", "T", "-"]
        encode x = encodeOverAlphabet (singleton (chunksOf 1 x)) alphabet
        decode x = Prelude.concat . Prelude.concat $ foldr (\x acc -> x : acc) [] $ decodeOverAlphabet x alphabet