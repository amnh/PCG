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

import           Bio.Sequence.Parsed
import           Bio.Sequence.Parsed.Class
import           Bio.Phylogeny.PhyloCharacter
import           Bio.Phylogeny.Graph.Data
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map    as M
import           Data.Matrix       (matrix)
import           Data.Maybe
import           Data.Set          (intersection)
import qualified Data.Set    as S  (fromList)
import           Data.Vector       (Vector)
import qualified Data.Vector as V
import           File.Format.Fasta (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus hiding (DNA, RNA)
import qualified File.Format.Nexus.Data as Nex
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot

dnaAlph, rnaAlph, aaAlph :: [String]
dnaAlph = pure <$> addOtherCases "AGCTRMWSKTVDHBNX?-"
rnaAlph = pure <$> addOtherCases "AGCURMWSKTVDHBNX?-"
aaAlph  = pure <$> addOtherCases "ABCDEFGHIKLMNPQRSTVWXYZ-"

addOtherCases :: String -> String
addOtherCases [] = []
addOtherCases (x:xs)
  | isLower x = (toUpper x) : x : addOtherCases xs
  | isUpper x = x : (toLower x) : addOtherCases xs
  | otherwise = x : addOtherCases xs

class Metadata a where
    unifyMetadata :: a -> [Vector CharInfo]

instance Metadata FastaParseResult where
    unifyMetadata = fmap makeEncodeInfo . unifyCharacters

instance Metadata TaxonSequenceMap where
    unifyMetadata = fmap makeEncodeInfo . unifyCharacters

instance Metadata FastcParseResult where
    unifyMetadata = fmap makeEncodeInfo . unifyCharacters

instance Metadata NewickForest where
    unifyMetadata _ = mempty

instance Metadata TntResult where
    unifyMetadata (Left _) = mempty
    unifyMetadata (Right withSeq) = pure $ V.map convertMeta (charMetaData withSeq)
        where
            convertMeta inMeta = 
                let defaultMeta = makeOneInfo (V.toList $ characterStates inMeta) --(True, 0)
                in defaultMeta {name = characterName inMeta, stateNames = characterStates inMeta, tcm = fromMaybe (tcm defaultMeta) (costTCM inMeta)}

instance Metadata TCM where
    unifyMetadata (TCM alph mat) = 
        let defaultMeta = makeOneInfo (toList alph)
        in pure $ pure (defaultMeta {tcm = mat})

instance Metadata VertexEdgeRoot where
    unifyMetadata _ = mempty

instance Metadata Nexus where
    unifyMetadata (Nexus (_, metas)) = pure $ V.map convertNexusMeta metas
        where
            convertNexusMeta inMeta = 
                let defaultMeta = makeOneInfo (Nex.alphabet inMeta)
                in  defaultMeta {name = Nex.name inMeta, aligned = Nex.isAligned inMeta, ignored = Nex.ignored inMeta, 
                                    tcm = fromMaybe (tcm defaultMeta) (transitionCosts <$> Nex.costM inMeta)}

-- TODO: Consider default metadata from the command structure
-- | Functionality to make char info from tree seqs
makeEncodeInfo :: TreeSeqs -> Vector CharInfo
makeEncodeInfo seqs = V.map makeOneInfo alphabets --allChecks
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
        getNodeAlphAt  Nothing     _     = mempty
        getNodeAlphAt (Just inSeq) soFar = foldr f soFar inSeq
          where
            f = (flip $ foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev)) 

        -- | Ensure that the gap char is present and correctly positioned in an alphabet
        setGapChar :: Alphabet -> Alphabet
        setGapChar inAlph = case elemIndex "-" inAlph of
            Just i -> take i inAlph ++ drop i inAlph ++ ["-"]
            Nothing -> inAlph ++ ["-"]

-- | Internal function to make one character info
makeOneInfo :: Alphabet -> CharInfo
makeOneInfo inAlph
    | inAlph `subsetOf` dnaAlph = DNA       "" False mempty mempty inAlph defaultMat False 1
    | inAlph `subsetOf` rnaAlph = RNA       "" False mempty mempty inAlph defaultMat False 1
    | inAlph `subsetOf`  aaAlph = AminoAcid "" False mempty inAlph mempty defaultMat False 1
    | otherwise = Custom "" False mempty inAlph mempty defaultMat False False 1
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

-- | /O(n*log n)/. The expression 'lhs `subsetOf` rhs' is 'True' /iff/
--   every element in 'lhs' is also an element is 'rhs'.
subsetOf :: Ord a => [a] -> [a] -> Bool
subsetOf list1 list2 = lhs `intersection` rhs == lhs
  where
    lhs = S.fromList list1
    rhs = S.fromList list2
