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
import qualified Data.Vector as V           (fromList)
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
sequentialAlign :: SeqConstraint s b => s -> s -> (s, Double, s, s, s)
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
        inferredParent' = simpleEncode inferredParent
        alignedParent'  = simpleEncode alignedParent
        alignment1'     = simpleEncode alignment1
        alignment2'     = simpleEncode alignment2
        inpSeq1'        = simpleDecode inpSeq1
        inpSeq2'        = simpleDecode inpSeq2
        (cost, alignment1, alignment2) = case FF.sequentialAlign 1 1 inpSeq1' inpSeq2' of
            Left e -> error e -- Better error handling later
            Right r -> r

-- | Simple encoding over a string just for you
simpleEncode :: (Monoid s, Show s, Bits s) => String -> s
simpleEncode inStr = (\x -> trace (show x) x) $ foldl simpleSetElem zeroBits vecStr
    where
        vecStr = V.fromList inStr

        ---- | Simple functionality to set an element in a bitvector
        --simpleSetElem :: Bits b => Int -> Char -> b -> b
        --simpleSetElem i char curBit = case elemIndex char "ACGT-" of
        --                                    Nothing -> curBit
        --                                    Just pos -> setBit curBit (pos + (i * 5))

                -- | Simple functionality to set an element in a bitvector
        simpleSetElem :: (Monoid b, Bits b) => b -> Char -> b
        simpleSetElem acc char = acc <> next 
          where
              next = case elemIndex char "ACGT-" of
                          Nothing  -> zeroBits
                          Just pos -> bit pos

simpleDecode :: (Show s, Bits s) => s -> String
simpleDecode = either (const "") id . parse escapeAllTheData "" . (\x -> trace x x) . show 

escapeAllTheData :: (MonadParsec s m Char) => m String
escapeAllTheData = (string' "Nothing" $> []) <|> (string' "Just " *> theVectorOfBitVectors)
  where
      theVectorOfBitVectors :: MonadParsec s m Char => m String
      theVectorOfBitVectors = between (char '[') (char ']') (bitVectorCombinator `sepBy1` char ',')
      bitVectorCombinator :: MonadParsec s m Char => m Char
      bitVectorCombinator = do 
            (_,integralValue) <- (,) <$> (char '[' *> integer <* char ']') <*> integer
            pure $ case (testBit integralValue) <$> [0..4] of
                    [True , False, False, False, False] -> 'A'
                    [False, True , False, False, False] -> 'C'
                    [False, False, True , False, False] -> 'G'
                    [False, False, False, True , False] -> 'T'
                    [False, False, False, False, True ] -> '-'
                    [False, True , True , True , False] -> 'B'
                    [True , False, True , True , False] -> 'D'
                    [True , True , False, True , False] -> 'H'
                    [False, False, True , True , False] -> 'K'
                    [True , True , False, False, False] -> 'M'
                    [True , True , True , True , False] -> 'N'
                    [True , False, True , False, False] -> 'R'
                    [False, True , True , False, False] -> 'S'
                    [True , True , True , False, False] -> 'V'
                    [True , False, False, True , False] -> 'W'
                    [False, True , False, True , False] -> 'Y'
                    _                                   -> '-'    

-- Handles any encoding scheme
{- simpleDecode''' :: SeqConstraint s b => s -> String
simpleDecode''' codedSeq = concat $ toDnaStream <$> bitValues
  where
    n           = numChars codedSeq
    bitValues :: CodedSequence s b => [b]
    bitValues   = willItCompile <$> [0..n-1]
    
    willItCompile :: CodedSequence s b => s -> Int -> b
    willItCompile s i =
        case grabSubChar s i of
            Nothing -> zeroBits
            Just b  -> b

    toDnaStream :: Bits b => b -> String
    toDnaStream = fmap decodeDNA . chunksOf 5 . toBitStream

    toBitStream :: Bits b => b -> [Bool]
    toBitStream = f 0
        where
            f :: Bits b => Int -> b -> [Bool]
            f i xs
                | zeroBits == xs = []
                | otherwise      = xs `testBit` i : f (i+1) (xs `clearBit` i)

    decodeDNA :: [Bool] -> Char
    decodeDNA x = case x of
                [True , False, False, False, False] -> 'A'
                [False, True , False, False, False] -> 'C'
                [False, False, True , False, False] -> 'G'
                [False, False, False, True , False] -> 'T'
                [False, False, False, False, True ] -> '-'
                [False, True , True , True , False] -> 'B'
                [True , False, True , True , False] -> 'D'
                [True , True , False, True , False] -> 'H'
                [False, False, True , True , False] -> 'K'
                [True , True , False, False, False] -> 'M'
                [True , True , True , True , False] -> 'N'
                [True , False, True , False, False] -> 'R'
                [False, True , True , False, False] -> 'S'
                [True , True , True , False, False] -> 'V'
                [True , False, False, True , False] -> 'W'
                [False, True , False, True , False] -> 'Y'
                _                                   -> '-'
-}