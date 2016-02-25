-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Conversion.Encoder
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common encoding functionality to many of the converters
--
-----------------------------------------------------------------------------

module File.Format.Conversion.Encoder where

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.PhyloCharacter
import           Bio.Sequence.Coded
import           Bio.Sequence.Parsed
import           Control.Monad
import           Data.BitVector        hiding (foldr, replicate, join)
import           Data.Int
import           Data.List             hiding (zipWith)
import qualified Data.Map.Lazy         as M
import           Data.Maybe
import           Data.Matrix.NotStupid              (matrix)
import           Data.Vector           hiding ((++), length, head, elem, foldr, map, maximum, replicate, null)
import qualified Data.Vector           as V   
import           Prelude               hiding (zipWith)

import Debug.Trace

type Encoded = EncodedSeq BitVector

dnaAlph, rnaAlph, aaAlph :: [String]
dnaAlph = ["A", "C", "G", "T", "-"] 
rnaAlph = ["A", "C", "G", "U", "-"]
aaAlph = ["R", "H", "K", "D", "E", "S", "T", "N", "Q", "C", "U", "G", "P", "A", "V", "L", "I", "M", "F", "Y", "W", "-"]

-- | Functionality to make char info from tree seqs
makeEncodeInfo :: TreeSeqs -> Vector CharInfo
makeEncodeInfo seqs = --trace ("makeEncodeInfo " ++ show alphabets)
                        zipWith makeOneInfo alphabets allChecks
    where
        alphabets = developAlphabets seqs
        allChecks = checkAlignLens seqs

-- | Internal function to create alphabets
developAlphabets :: TreeSeqs -> Vector Alphabet
developAlphabets inSeqs = V.map sort $ M.foldr (zipWith getNodeAlphAt) initializer inSeqs
    where
        someSeq = head $ M.elems inSeqs
        initializer = if null someSeq then mempty
                        else V.replicate (V.length someSeq) mempty

        getNodeAlphAt :: Maybe ParsedSeq -> Alphabet -> Alphabet
        --getNodeAlphAt inSeq soFar | trace ("getNodeAlphAt " ++ show inSeq ++ " with accum " ++ show soFar) False = undefined
        getNodeAlphAt inSeq soFar
            | isNothing inSeq = mempty
            | otherwise =  V.foldr (\cIn cPrev -> foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev) cPrev cIn) soFar (fromJust inSeq)

-- | Internal function to make one character info
makeOneInfo :: Alphabet -> (Bool, Int) -> CharInfo
makeOneInfo inAlph (isAligned, seqLen)
    | inAlph `subsetOf` dnaAlph = DNA "" isAligned masks mempty alph defaultMat False
    | inAlph `subsetOf` rnaAlph = RNA "" isAligned masks mempty alph defaultMat False
    | inAlph `subsetOf` aaAlph = AminoAcid "" isAligned masks mempty alph defaultMat False
    | otherwise = Custom "" isAligned masks mempty alph defaultMat False False
        where 
            defaultMat = matrix (length inAlph) (length inAlph) (const 1)
            alph = V.fromList inAlph
            masks = generateMasks (length inAlph) seqLen

            generateMasks :: Int -> Int -> (Encoded, Encoded)
            generateMasks alphLen sLen = 
                let
                    unit = replicate (alphLen - 1) False ++ [True]
                    numUnits = (div sLen alphLen) + 1
                    periodic = fromBits $ foldr (++) [] (replicate numUnits unit)
                    occupancy = fromBits $ replicate (alphLen * sLen) True
                in (Just $ V.singleton $ occupancy, Just $ V.singleton $ periodic)

checkAlignLens :: TreeSeqs -> Vector (Bool, Int)
checkAlignLens = M.foldr matchLens mempty
    where
        matchLens :: ParsedSequences -> Vector (Bool, Int) -> Vector (Bool, Int)
        matchLens curSeqs prevVals
            | V.null prevVals = V.map makeVal curSeqs
            | otherwise = V.zipWith checkVal curSeqs prevVals
                where
                    checkVal v pv = if isNothing v then (False, 0)
                                        else (V.length (fromJust v) == snd pv, maximum [V.length $ fromJust v, snd pv])
                    makeVal v = if isNothing v then (False, 0)
                                    else (True, V.length $ fromJust v)

subsetOf :: (Ord a) => [a] -> [a] -> Bool
subsetOf list1 list2 = foldr (\e acc -> acc && e `elem` list2) True list1

encodeIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
encodeIt = zipWith (\s info -> join $ (flip encodeOverAlphabet) (V.toList $ alphabet info) <$> s)

packIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
packIt = encodeIt

