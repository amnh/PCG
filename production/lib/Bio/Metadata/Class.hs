-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Metadata.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for a set of metadata
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Metadata.Class where

import Bio.Sequence.Parsed
import Bio.Sequence.Parsed.Class
import Bio.Phylogeny.PhyloCharacter
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Matrix (matrix)
import Data.List
import qualified Data.Map as M
import File.Format.Fasta (FastaParseResult)
import File.Format.Fastc
import File.Format.Newick
import File.Format.TNT


dnaAlph, rnaAlph, aaAlph :: [String]
dnaAlph = ["A", "C", "G", "T", "-"] 
rnaAlph = ["A", "C", "G", "U", "-"]
aaAlph = ["R", "H", "K", "D", "E", "S", "T", "N", "Q", "C", "U", "G", "P", "A", "V", "L", "I", "M", "F", "Y", "W", "-"]

class Metadata a where
    unifyMetadata :: Monoid b => a -> [Vector (PhyloCharacter b)]

instance Metadata FastaParseResult where
    unifyMetadata inChars = map makeEncodeInfo (unifyCharacters inChars)

instance Metadata FastcParseResult where
    unifyMetadata inChars = map makeEncodeInfo (unifyCharacters inChars)

instance Metadata NewickForest where
    unifyMetadata = mempty

instance Metadata TntResult where
    unifyMetadata (Left _) = mempty
    unifyMetadata (Right withSeq) = pure $ V.map convertMeta (charMetaData withSeq)
        where
            convertMeta inMeta = 
                let defaultMeta = makeOneInfo (V.toList $ characterStates inMeta) (True, 0)
                in defaultMeta {name = characterName inMeta, stateNames = characterStates inMeta}

-- | Functionality to make char info from tree seqs
makeEncodeInfo :: Monoid b => TreeSeqs -> Vector (PhyloCharacter b)
makeEncodeInfo seqs = V.zipWith makeOneInfo alphabets allChecks
    where
        alphabets = developAlphabets seqs
        allChecks = checkAlignLens seqs

-- | Internal function to create alphabets
developAlphabets :: TreeSeqs -> Vector Alphabet
developAlphabets inSeqs = V.map setGapChar $ V.map sort $ M.foldr (V.zipWith getNodeAlphAt) initializer inSeqs
    where
        someSeq = head $ M.elems inSeqs
        initializer = if null someSeq then mempty
                        else V.replicate (V.length someSeq) mempty

        getNodeAlphAt :: Maybe ParsedSeq -> Alphabet -> Alphabet
        --getNodeAlphAt inSeq soFar | trace ("getNodeAlphAt " ++ show inSeq ++ " with accum " ++ show soFar) False = undefined
        getNodeAlphAt inSeq soFar
            | isNothing inSeq = mempty
            | otherwise =  V.foldr (flip $ foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev)) soFar (fromJust inSeq)

        -- | Ensure that the gap char is present and correctly positioned in an alphabet
        setGapChar :: Alphabet -> Alphabet
        setGapChar inAlph = case elemIndex "-" inAlph of
            Just i -> take i inAlph ++ drop i inAlph ++ ["-"]
            Nothing -> inAlph ++ ["-"]

-- | Internal function to make one character info
makeOneInfo :: Monoid b => Alphabet -> (Bool, Int) -> (PhyloCharacter b)
makeOneInfo inAlph (isAligned, seqLen)
    | inAlph `subsetOf` dnaAlph = DNA "" isAligned mempty mempty inAlph defaultMat False
    | inAlph `subsetOf` rnaAlph = RNA "" isAligned mempty mempty inAlph defaultMat False
    | inAlph `subsetOf` aaAlph = AminoAcid "" isAligned mempty inAlph mempty defaultMat False
    | otherwise = Custom "" isAligned mempty inAlph mempty defaultMat False False
        where 
            defaultMat = matrix (length inAlph) (length inAlph) (const 1)
            --masks = generateMasks (length inAlph) seqLen isAligned

            --generateMasks :: Int -> Int -> Bool -> (Encoded, Encoded)
            --generateMasks alphLen sLen isAligned 
            --    | isAligned = 
            --        let 
            --            periodic = fromBits $ concat (replicate sLen unit)
            --            occupancy = fromBits $ replicate (alphLen * sLen) True
            --        in (Just $ V.singleton occupancy, Just $ V.singleton periodic)
            --    | otherwise = 
            --        let
            --            periodic = fromBits <$> replicate sLen unit
            --            occupancy = fromBits <$> replicate sLen (replicate alphLen True)
            --        in (Just $ V.fromList occupancy, Just $ V.fromList periodic)
            --        where
            --            unit = replicate (alphLen - 1) False ++ [True]

-- | Internal function to check whether sequences might be aligned
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
